package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._


case object CheckJobDone
case object CheckJobStart
case class BuildProject(name: String, args: Map[String,String], watcher: ActorRef)
case class BuildResult(success: Boolean, url: String)
  
/** An actor that can build jenkins jobs. */
class JenkinsJobBuilder(val api: JenkinsAPI)  extends Actor {
  
  def receive: Receive = {
    case b: BuildProject => buildAndWatch(b)
  }
  
  /** Fires off the job, finds it in the API and then spawns a watcher to check for its completion. */
  private def buildAndWatch(b: BuildProject): Unit = {
    // Build and then create an actor to wait for its start.
    api.buildJob(b.name, b.args)
    val startWatcher = context.system.actorOf(Props().withCreator(new JobStartWatcher(api, b)))
    context.system.scheduler.scheduleOnce(30 seconds, startWatcher, CheckJobStart)
  }
}

/** Helper trait that just has a nice DoNothing behavior for actors that eventually ignore everything */
trait DoNothingActor extends Actor {
  protected final val DoNothing: Receive = {
    case CheckJobDone  => ()
    case CheckJobStart => ()
  }
}

/** A class that watches for a job to begin on jenkins and then
 * passes off to another actor to watch for the job to complete. 
 */
class JobStartWatcher(api: JenkinsAPI, b: BuildProject) extends Actor with DoNothingActor {
  def receive: Receive = {
    case CheckJobStart => 
      findBuild match {
        case Some(number) =>
          // Create a "done" watcher and let him go to town on the specific build.
          val watcher = context.actorOf(Props(  new JobWatcher(api, b.name, number, b.watcher)))
          context.system.scheduler.scheduleOnce(1 minutes, watcher, CheckJobDone)
          context.become(DoNothing)
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
  
  private final def findBuild =
    (for {
        status <- api.buildStatusForJob(b.name).view
        if isSame(status.actions.parameters, b.args)
     } yield status.number) headOption
}

/** An actor that watches a specific job and returns its status when the job is completed. */
class JobWatcher(api: JenkinsAPI, jobname: String, buildnumber: String, watcher: ActorRef) extends Actor with DoNothingActor {
  def receive: Receive = {
    case CheckJobDone => 
      val status = jobStatus
      if(!status.building) {
        watcher ! BuildResult(status.isSuccess, status.url)
        context.become(DoNothing)
      } else context.system.scheduler.scheduleOnce(1 minutes, self, CheckJobDone)
  }
  private def jobStatus = api.buildStatus(jobname, buildnumber)
  /** A timeout timer that wakes us up to check build status. */
}
