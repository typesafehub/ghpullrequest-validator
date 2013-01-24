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

  // start the job on jenkins
  api.buildJob(b.job, b.args)

  // Set timeout to check when the job has started.
  context setReceiveTimeout (30 seconds)

  def receive: Receive = {
    case _ =>
      findBuild match {
        case Some(status) =>
          // Create a "done" watcher and let him go to town on the specific build.
          // Pass the props to our parent so he "owns" the watcher when we die.
          b.watcher ! BuildStarted(status.url)
          jenkinsService ! JobStarted(b, status)
          context stop self
        case None        =>
          // Could not find the build yet, let's look again soon.
          context setReceiveTimeout (1 minutes)
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
      status.actions.parameters.map{case Param(n, v) => (n, v)}.toMap == b.args
    }

    jobsForThisPR.headOption match {
      case res@Some(status) => log.debug("foundBuild "+ (b.job, b.args.get("pullrequest"), status))
        res
      case _ => log.debug("didn't find current build "+ (b.job, b.args.get("pullrequest"), jobsForThisPR))
        None
    }
  }
}