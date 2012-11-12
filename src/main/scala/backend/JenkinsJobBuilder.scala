package backend

import akka.actor.{Actor, ActorRef, ActorSystem, ActorLogging, Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._
import rest.jenkins.BuildStatus

// External Messages
case class BuildProject(job: JenkinsJob, args: Map[String,String], watcher: ActorRef)
case class BuildStarted(url: String)
case class BuildResult(success: Boolean, url: String)


// Internal messages
case class JobStarted(b: BuildProject, status: BuildStatus)
case class JobFinished(b: BuildProject, status: BuildStatus)
  
/** An actor that can build jenkins jobs. */
class JenkinsJobBuilder(val api: JenkinsAPI)  extends Actor with ActorLogging {
  // Pretty simple implementation, just spawn someone else up to start a job.
  // TODO - Detect failure and handle them....
  def receive: Receive = {
    case build: BuildProject => 
      val me = self
      log.debug("Starting job watcher for: " +build.job.name)
      context actorOf Props(new JenkinsJobStartWatcher(api, build, me))

    case JobStarted(build, status) =>
      build.watcher ! BuildStarted(status.url)
      log.debug("Job started: " + build.job.name + "-" + status.number)
      context.actorOf(
          Props(new JenkinsJobWatcher(api, build, status.number, self)),
          build.job.name + "-" + status.number)

    case JobFinished(build, status) =>
      build.watcher ! BuildResult(status.isSuccess, status.url)
  }
}