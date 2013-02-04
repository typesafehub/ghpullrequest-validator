package backend

import akka.actor.{Actor, ActorRef, ActorSystem, ActorLogging, Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._
import rest.jenkins.BuildStatus

// External Messages
case class BuildCommit(sha: String, job: JenkinsJob, args: Map[String,String], force: Boolean, noop: Boolean, commenter: ActorRef)
case class BuildStarted(url: String)
case class BuildResult(status: BuildStatus)
case object BuildQueued

// Internal messages
case class JobStarted(b: BuildCommit, status: BuildStatus)
  
/** An actor that can build jenkins jobs. */
class JenkinsJobBuilder(val api: JenkinsAPI)  extends Actor with ActorLogging {
  // Pretty simple implementation, just spawn someone else up to start a job.
  // TODO - Detect failure and handle them....
  def receive: Receive = {
    case build: BuildCommit =>
      val me = self
      log.debug("Starting job watcher for: "+ (build.job.name, build.sha))
      context actorOf Props(new JenkinsJobStartWatcher(api, build, me))

    case JobStarted(build, status) =>
      log.debug("Job started: " + build.job.name + "-" + status.number)
      context.actorOf(
          Props(new JenkinsJobWatcher(api, build, status.number)),
          build.job.name + "-" + status.number)
  }
}