package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._


case object CheckJobAgain
case class BuildProject(name: String, args: Map[String,String], watcher: ActorRef)
case class BuildResult(success: Boolean, url: String)
  
/** An actor that can build jenkins jobs. */
class JenkinsJobBuilder(val api: JenkinsAPI)  extends Actor {
  
  def receive: Receive = {
    case b @ BuildProject(_, _, _) => buildAndWatch(b)
  }
  
  /** Fires off the job, finds it in the API and then spawns a watcher to check for its completion. */
  private def buildAndWatch(b: BuildProject): Unit = {
    def isSame(params: List[rest.jenkins.Param], args: Map[String,String]): Boolean = {
      val allsame = (for {
        param <- params
        other <- args get param.name
      } yield other == param.value) forall identity
      // TODO - does this handle defaults?
      allsame //&& params.size == args.size
    }
    
    // Build and then wait for job to start...
    api.buildJob(b.name, b.args)
    
    // TODO - Rather than checking in 30 seconds, just keep polling until we find the job...
    val system = context.system
    system.scheduler.scheduleOnce(30 seconds) {
      val build = (for {
        status <- api.buildStatusForJob(b.name).view
        if isSame(status.actions.parameters, b.args)
      } yield status.number) headOption
      
      build match {
        case Some(number) =>
          val watcher = system.actorOf(Props().withCreator(new JobWatcher(api, b.name, number, b.watcher)))
          system.scheduler.scheduleOnce(1 minutes, watcher, CheckJobAgain)
        case None => 
          b.watcher ! BuildResult(false, "Failed to start build!")
      }  
    }
  }
}

/** An actor that watches a specific job and returns its status when the job is completed. */
class JobWatcher(api: JenkinsAPI, jobname: String, buildnumber: String, watcher: ActorRef) extends Actor {
  def receive: Receive = {
    case CheckJobAgain => 
      val status = jobStatus
      if(!status.building) {
        watcher ! BuildResult(status.isSuccess, status.url)
        context.become(DoNothing)
      } else context.system.scheduler.scheduleOnce(1 minutes, self, CheckJobAgain)
  }
  private def DoNothing: Receive = {
    case CheckJobAgain => ()
  }
  private def jobStatus = api.buildStatus(jobname, buildnumber)
  /** A timeout timer that wakes us up to check build status. */
}
