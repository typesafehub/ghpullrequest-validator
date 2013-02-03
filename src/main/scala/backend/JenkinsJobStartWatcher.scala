package backend

import akka.actor.{Actor, ActorRef, ActorSystem, Props, ActorLogging, ReceiveTimeout}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._



/** A class that watches for a job to begin on jenkins and then
 * notifies the jenkinsService that the job has started.
 */
// TODO - We need a new way to detect a job has started...
class JenkinsJobStartWatcher(api: JenkinsAPI, b: BuildCommit, jenkinsService: ActorRef) extends Actor with ActorLogging {
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

  def receive: Receive = { case ReceiveTimeout =>
    jenkinsStatus match {
      case JQueue =>
        // be patient when there's a queue, don't even start a job as we won't be able to tell whether it started
      case JRetry =>
        retryCount += 1
        // we haven't started our own build yet, but maybe an older one is running
        // watch its result, but still start our own
        val runningBuilds = ourJobs.filter(_.building) match {
          case bs if bs.isEmpty => ourJobs.headOption.toStream // if no running builds, just look at the most recent one
          case bs => bs
        }

        runningBuilds.foreach { status =>
          log.debug("found running build "+ (b.job, "#"+b.args.get("pullrequest"), status))
          jenkinsService ! JobStarted(b, status)
          b.commenter ! BuildStarted(status.url)
        }

        // if there's a running job for this commit and this job, that's good enough (unless we encountered an explicit PLS REBUILD --> b.force)
        if (runningBuilds.nonEmpty && !b.force) context stop self
        else {
          api.buildJob(b.job, b.args)
          log.debug("Started job for #"+ b.args.get("pullrequest") +" --> "+ b.job.name +" args: "+ b.args)
          context setReceiveTimeout (1 minutes)
        }

      case JStop =>
        log.debug("timed out finding build "+ (b.job, b.args.get("pullrequest"), api.jobInfo(b.job)))
        jenkinsService ! b // tell the service to rebuild
        context stop self // stop looking for the job to start
    }
  }

  
  def ourJobs =
    api.buildStatusForJob(b.job).filter { status =>
      status.actions.parameters.collect{case rest.jenkins.Param(n, v) if b.args.isDefinedAt(n) => (n, v)}.toMap == b.args
    }


  // Finds most recent build for this PR & merge branch
  // we can't be sure it's us who started the build, so just assume the most recent build was us
  def findBuild = {
    val activeJobs = ourJobs.filter(_.building)
    activeJobs.headOption match {
      case res@Some(status) => log.debug("foundBuild "+ (b.job, b.args.get("pullrequest"), status))
        res
      case _ => log.debug("didn't find current build "+ (b.job, b.args.get("pullrequest"), activeJobs))
        None
    }
  }
}