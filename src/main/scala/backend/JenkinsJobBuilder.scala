package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._
import rest.jenkins.BuildStatus

// External Messages
case class BuildProject(name: String, args: Map[String,String], watcher: ActorRef)
case class BuildStarted(url: String)
case class BuildResult(success: Boolean, url: String)


// Internal messages
case class JobStarted(b: BuildProject, status: BuildStatus)
case class JobFinished(b: BuildProject, status: BuildStatus)
  
/** An actor that can build jenkins jobs. */
class JenkinsJobBuilder(val api: JenkinsAPI)  extends Actor {
  // Pretty simple implementation, just spawn someone else up to start a job.
  // TODO - Detect failure and handle them....
  def receive: Receive = {
    case b: BuildProject => 
      val me = self
      System.err.println("Starting job watcher for: " +b.name)
      context actorOf Props(new JenkinsJobStartWatcher(api, b, me))

    case JobStarted(build, status) =>
      build.watcher ! BuildStarted(status.url)
      System.err.println("Job started: " + build.name + "-" + status.number)
      context.actorOf(
          Props(new JenkinsJobWatcher(api, build, status.number, self)),
          build.name + "-" + status.number)

    case JobFinished(build, status) =>
      build.watcher ! BuildResult(status.isSuccess, status.url)
  }
  
  override def postRestart(t: Throwable): Unit = {
    System.err.println("Jenkins Job Builder restarted: " + t)
  }
  
  override def postStop(): Unit = {
    System.err.println("Jenkins Job Builder stopped.")
  }
}