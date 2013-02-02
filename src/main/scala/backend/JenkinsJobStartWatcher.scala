package backend

import akka.actor.{Actor, ActorRef, ActorSystem, Props, ActorLogging, ReceiveTimeout}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._



/** A class that watches for a job to begin on jenkins and then
 * notifies the jenkinsService that the job has started.
 */
// TODO - We need a new way to detect a job has started...
class JenkinsJobStartWatcher(api: JenkinsAPI, b: BuildProject, jenkinsService: ActorRef) extends Actor with ActorLogging {
  // TODO - Use better time library....

  // Set timeout to check when the job has started.
  context setReceiveTimeout (30 seconds)

  private[this] var retryCount = 0
  private val MAX_RETRIES = 10

  sealed abstract class JenkinsStatus
  case object JQueue extends JenkinsStatus
  case object JRetry extends JenkinsStatus
  case object JStop  extends JenkinsStatus
  def jenkinsStatus: JenkinsStatus =
    api.jobInfo(b.job).queueItem match {
      // there's a queue, be patient as jenkins doesn't tell us about queued jobs
      case Some(rest.jenkins.QueueItem(true)) =>
        log.debug("Retrying -- queue "+ b.job +" #"+ b.args.get("pullrequest"))
        JQueue
      // no queue, if don't start finding it by now, something went wrong
      case _ =>
        log.debug("Retrying -- no queue for "+ b.job +" #"+ b.args.get("pullrequest"))
        if (retryCount < MAX_RETRIES) JRetry else JStop
    }

  // a after b or within 2 minutes (to account for clock skew)
  private def closeEnough(a: Long, b: Long) =
    a >= b || (b - a)/1000 <= 120

  private[this] var startDate: Option[Long] = None
  def receive: Receive = { case ReceiveTimeout =>
    jenkinsStatus match {
      case JQueue =>
        // be patient when there's a queue, don't even start a job as we won't be able to tell whether it started
      case JRetry =>
        // try to start the job on jenkins once
        startDate match {
          case None =>
            startDate = Some(System.currentTimeMillis)
            api.buildJob(b.job, b.args)
            log.debug("Started job "+ b.job.name +"args: "+ b.args)
          case Some(start) =>
            retryCount += 1
            findBuild match {
              // TODO: make sure it's the build we triggered by adding a UID to the build params
              // for now, assume all builds started close to when we tried starting our job are ours
              case Some(status) if closeEnough(status.timestamp.toLong, start) =>
                b.commenter ! BuildStarted(status.url)

                // Create a "done" watcher and let him go to town on the specific build.
                // Pass the props to our parent so he "owns" the watcher when we die.
                jenkinsService ! JobStarted(b, status)
                context stop self
              case s =>
                if (s.nonEmpty) log.debug("found build but not ours? "+ (b.job, "#"+b.args.get("pullrequest"), s.get))
                // no running job found
                // start looking for it to appear
                context setReceiveTimeout (1 minutes)
            }
          }

      case JStop =>
        log.debug("timed out finding build "+ (b.job, b.args.get("pullrequest"), api.jobInfo(b.job)))
        jenkinsService ! b // tell the service to rebuild
        context stop self // stop looking for the job to start
    }
  }

  // // TODO - Look into adding UUID to build parameters....
  // private final def isSame(params: List[rest.jenkins.Param], args: Map[String,String]): Boolean = {
  //   val allsame = (for {
  //     param <- params
  //     other <- args get param.name
  //   } yield other == param.value) forall identity
  //   // TODO - does this handle defaults?
  //   // log.debug("isSame: "+(params, args, allsame))
  //   allsame //&& params.size == args.size
  // }
  
  // Finds most recent build for this PR & merge branch
  // we can't be sure it's us who started the build, so just assume the most recent build was us
  def findBuild = {
    import rest.jenkins.Param
    val buildStati = api.buildStatusForJob(b.job)
    val jobsForThisPR = buildStati.filter { status =>
      status.actions.parameters.collect{case Param(n, v) if b.args.isDefinedAt(n) => (n, v)}.toMap == b.args
    }

    jobsForThisPR.headOption match {
      case res@Some(status) => log.debug("foundBuild "+ (b.job, b.args.get("pullrequest"), status))
        res
      case _ => log.debug("didn't find current build "+ (b.job, b.args.get("pullrequest"), jobsForThisPR))
        None
    }
  }
}