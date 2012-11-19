package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props,ReceiveTimeout}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._



/** A class that watches for a job to begin on jenkins and then
 * notifies the jenkinsService that the job has started.
 */
// TODO - We need a new way to detect a job has started...
class JenkinsJobStartWatcher(api: JenkinsAPI, b: BuildProject, jenkinsService: ActorRef) extends Actor {
  // TODO - Use better time library....
  
  // 1. capture the latest buld time, or just the beginning of time if we have none.
  // 2. start the job on jenkins
  // 3. Set timeout to check when the job has started.
  val myTime = 
    (api.buildStatusForJob(b.job).view.headOption
     map (_.timestampDate)
     getOrElse new java.util.Date(0))
  api.buildJob(b.job, b.args)
  context setReceiveTimeout (30 seconds)
  
  def receive: Receive = {
    case ReceiveTimeout => 
      findBuild match {
        case Some((number, status)) =>
          // Create a "done" watcher and let him go to town on the specific build.
          // Pass the props to our parent so he "owns" the watcher when we die.
          jenkinsService ! JobStarted(b, status)
          context stop self
        case None        =>
          // Could not find the build yet, let's look again soon.
          context setReceiveTimeout (1 minutes)
      }
  }
  // TODO - Look into adding UUID to build parameters....
  private final def isSame(params: List[rest.jenkins.Param], args: Map[String,String]): Boolean = {
    val allsame = (for {
      param <- params
      other <- args get param.name
    } yield other == param.value) forall identity
    // TODO - does this handle defaults?
    allsame //&& params.size == args.size
  }
  
  // Finds our build, making sure timestamp is after we started the build,
  // and all the variables line up.
  private final def findBuild =
    (for {
        // Make sure this is a new build.
        status <- api.buildStatusForJob(b.job).view takeWhile (_.timestampDate after myTime)
        if isSame(status.actions.parameters, b.args)
     } yield status.number -> status) headOption
}