package backend

import akka.actor.{Actor, ActorRef, ActorSystem, ActorLogging, Props}
import rest.jenkins.{API => JenkinsAPI}
import rest.jenkins.BuildStatus

// External Messages
case class BuildCommit(sha: String, job: JenkinsJob, args: Map[String,String], force: Boolean, noop: Boolean, commenter: ActorRef) {
  assert(!(force && noop), "Force and noop cannot both be true.")

  override def toString = s"$actionString ${job.name} for #${args("pullrequest")}@${args("sha").take(6)}"
  def actionString = if (force) "Rebuild" else if (noop) "Synch" else "Build"
}
case class BuildStarted(url: String)
case class BuildResult(status: BuildStatus)
case object BuildQueued

// Internal messages
case class JobStarted(b: BuildCommit, status: BuildStatus)

object JenkinsJobBuilder {
  def props(api: JenkinsAPI): Props = Props(classOf[JenkinsJobBuilder], api)
}

/** An actor that can build jenkins jobs. */
class JenkinsJobBuilder(val api: JenkinsAPI)  extends Actor with ActorLogging {
  var jswCounter = 0
  var jwCounter = 0
  // Pretty simple implementation, just spawn someone else up to start a job.
  // TODO - Detect failure and handle them....
  def receive: Receive = {
    case build: BuildCommit =>
      val me = self
      log.info(s"Watching for $build.")
      context.actorOf(JenkinsJobStartWatcher.props(api, build, me), s"job-start-watcher-${build.job.name}@${build.sha}-${jswCounter += 1; jswCounter}")

    case JobStarted(build, status) =>
      log.info(s"Started: $build --> $status")
      context.actorOf(
        JenkinsJobWatcher.props(api, build, status.number), // , build.job.name + "-" + status.number <-- not unique, not needed?
        s"job-watcher-${build.job.name}@${build.sha}-${jwCounter += 1; jwCounter}"
    )
  }
}
