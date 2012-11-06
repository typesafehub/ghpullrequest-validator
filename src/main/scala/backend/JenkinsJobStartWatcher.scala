package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._



/** A class that watches for a job to begin on jenkins and then
 * passes off to another actor to watch for the job to complete. 
 */
class JenkinsJobStartWatcher(api: JenkinsAPI, b: BuildProject) extends Actor {
  def receive: Receive = {
    case CheckJobStart => 
      findBuild match {
        case Some((number, status)) =>
          // Create a "done" watcher and let him go to town on the specific build.
          val watcher = context.actorOf(Props(new JenkinsJobWatcher(api, b.name, number, b.watcher)))
          context.system.scheduler.scheduleOnce(1 minutes, watcher, CheckJobDone)
          b.watcher ! BuildStarted(status.url)
          context stop self
        case None        =>
          // Could not find the build yet, let's look again soon.
          context.system.scheduler.scheduleOnce(1 minutes, self, CheckJobStart)
      }
  }
  
  private final def isSame(params: List[rest.jenkins.Param], args: Map[String,String]): Boolean = {
    val allsame = (for {
      param <- params
      other <- args get param.name
    } yield other == param.value) forall identity
    // TODO - does this handle defaults?
    allsame //&& params.size == args.size
  }
  
  // TODO - Check start time too....
  private final def findBuild =
    (for {
        status <- api.buildStatusForJob(b.name).view
        if isSame(status.actions.parameters, b.args)
     } yield status.number -> status) headOption
}