package backend

import akka.actor.{ Actor, ActorRef, ActorSystem, Props, ActorLogging, ReceiveTimeout }
import rest.jenkins.{ API => JenkinsAPI }
import scala.concurrent.duration._
import rest.jenkins.BuildStatus

object JenkinsJobStartWatcher {
  def props(api: JenkinsAPI, b: BuildCommit, jenkinsService: ActorRef): Props =
    Props(classOf[JenkinsJobStartWatcher], api, b, jenkinsService)
}

/** A class that watches for a job to begin on jenkins and then
 *  notifies the jenkinsService that the job has started.
 */
// TODO - We need a new way to detect a job has started...
class JenkinsJobStartWatcher(api: JenkinsAPI, b: BuildCommit, jenkinsService: ActorRef) extends Actor with ActorLogging {
  // Set timeout to check when the job has started.
  context setReceiveTimeout (20 seconds)

  // we only try to start a build once, if that fails, we'll time out and try again
  // we never start one if the BuildCommit message resulted from a PLS SYNCH (`b.noop`)
  private[this] var startBuildTODO = !b.noop
  private[this] var retryCount = 12 // if jenkins hasn't at least queued our job 4 mins (= 12 timeouts) later, something is wrong and we retry

  private val seenBuilds = collection.mutable.HashSet[String]()
  private def updateOtherActors(relevantBuilds: Stream[BuildStatus]) = {
    // only look at most recent job not seen before
    // (they are ordered by the jenkins in API with the most recent job at the head)
    relevantBuilds.filterNot(bs => seenBuilds(bs.url)).headOption foreach { status =>
      seenBuilds += status.url
      if (status.building) {
        jenkinsService ! JobStarted(b, status)
        b.commenter ! BuildStarted(status.url)
      }
      else if (status.queued) b.commenter ! BuildQueued
      else b.commenter ! BuildResult(status)
    }
  }

  def receive: Receive = {
    case ReceiveTimeout =>
      // all build statuses for this PR, sha, mergebranch (for each arg in b.args, there must be a matching param in the job)
      val allBuilds: Stream[BuildStatus] = api.buildStatusForJob(b.job, b.args)

      // building or queued
      val currentBuilds: Stream[BuildStatus] = allBuilds.filter(bs => bs.building || bs.queued)

      // only consider building or queued jobs if either:
      //   - we're _not_ reacting to a PLS SYNCH (b.noop)
      //   - we're reacting to a PLS REBUILD (b.force),
      //   - we haven't been looking that long (retryCount > 6)
      // else assume that the build we're looking for may have ended,
      // and consider all builds with the expected parameters
      val reportedBuilds: Stream[BuildStatus] =
        if (!b.noop && (b.force || retryCount > 6)) currentBuilds else allBuilds

      updateOtherActors(reportedBuilds)

      // if (retryCount < 10 && reportedBuilds.isEmpty)
      //   log.warning(s"No builds to report for $b.\nAll jobs: " + allBuilds)

      // on our first timeout, we try to start the build if necessary
      if (startBuildTODO) {
        startBuildTODO = false

        // if we're running in force mode or we haven't found a running/queued build
        if (b.force || currentBuilds.isEmpty) {
          log.debug(s"Attempting $b")

          api.buildJob(b.job, b.args)
        }
      }
      else if (reportedBuilds.isEmpty) {
        retryCount -= 1
        if (retryCount == 0) {
          log.error(s"Failed to start or find started $b")

          jenkinsService ! b // retry the build
          context stop self
        }
      }
      // we reported on a build
      else {
        log.info(s"$b --> ${reportedBuilds take 1}")
        context stop self
      }
  }
}