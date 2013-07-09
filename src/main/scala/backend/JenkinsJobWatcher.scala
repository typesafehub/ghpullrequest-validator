package backend

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props, ReceiveTimeout }
import rest.jenkins.{ API => JenkinsAPI }
import scala.concurrent.duration._

/** An actor that watches a specific job and returns its status when the job is completed. */
class JenkinsJobWatcher(api: JenkinsAPI, build: BuildCommit, buildnumber: String) extends Actor with ActorLogging {
  log.info(s"Waiting for end of $build [$buildnumber].")
  private var _backoff = 1
  def backoff: Int =
    if (_backoff >= 8) 8
    else {
      val res = _backoff
      _backoff *= 2
      res
    }

  def receive: Receive = {
    case ReceiveTimeout =>
      api.buildStatus(build.job, buildnumber) foreach { status =>
        if (status.building) context setReceiveTimeout ((backoff * 30) seconds)
        else {
          log.info(s"Done: $build --> $status.")

          build.commenter ! BuildResult(status)
          context stop self
        }
      }
  }

  /** A timeout timer that wakes us up to check build status. */
  context setReceiveTimeout (15 seconds)
}
