package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._


case object CheckJobDone
case object CheckJobStart
case class BuildProject(name: String, args: Map[String,String], watcher: ActorRef)
case class BuildStarted(url: String)
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
    val startWatcher = context.system.actorOf(Props().withCreator(new JenkinsJobStartWatcher(api, b)))
    context.system.scheduler.scheduleOnce(30 seconds, startWatcher, CheckJobStart)
  }
}