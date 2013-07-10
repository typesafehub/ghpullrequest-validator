package backend

import akka.actor.{ Actor, ActorRef, ActorSystem, Props, ActorLogging, ReceiveTimeout }
import rest.jenkins.{ API => JenkinsAPI }
import scala.concurrent.duration._
import rest.jenkins.BuildStatus

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
    val newlyDiscovered = relevantBuilds.filterNot(bs => seenBuilds(bs.url))

    val (buildingOrQueued, finished) = newlyDiscovered.partition(st => st.building || st.queued)

    buildingOrQueued foreach { status =>
      if (status.queued)
        b.commenter ! BuildQueued
      else {
        jenkinsService ! JobStarted(b, status)
        b.commenter ! BuildStarted(status.url)
      }
    }

    // finished jobs are handled after sending BuildStarted so that (hopefully) this status is more recent
    // NOTE: these are expected to be empty, and are definitely empty when retryCount > 6 (the first half or this actor's lifespan)
    finished foreach { status =>
      b.commenter ! BuildResult(status)
    }

    seenBuilds ++= newlyDiscovered.map(_.url)
  }

  def receive: Receive = {
    case ReceiveTimeout =>
      // all build statuses for this PR, sha, mergebranch (for each arg in b.args, there must be a matching param in the job)
      val allBuilds = api.buildStatusForJob(b.job, b.args)

      // building or queued
      val currentBuilds = allBuilds.filter(bs => bs.building || bs.queued)

      // only consider building or queued jobs if either:
      //   - we're reacting to a PLS REBUILD (b.force),
      //   - we haven't been looking that long (retryCount > 6)
      // else, if we're not running in forced mode, and have been looking for a while,
      // assume the build we're looking for may have ended, and consider all builds with the expected parameters
      val reportedBuilds =
        if (b.force || retryCount > 6) currentBuilds else allBuilds

      updateOtherActors(reportedBuilds)

      if (retryCount > 6 && currentBuilds.isEmpty)
        log.warning(s"No active builds for $b.\nAll jobs: " + allBuilds)

      // on our first timeout, we try to start the build if necessary
      if (startBuildTODO) {
        startBuildTODO = false

        // if we're running in force mode or we haven't found a running/queued build
        if (b.force || currentBuilds.isEmpty) {
          log.debug(s"Attempting $b")

          api.buildJob(b.job, b.args)
        }
      }
      // we started a build already, but no builds found
      else if (currentBuilds.isEmpty) {
        retryCount -= 1
        if (retryCount == 0) {
          jenkinsService ! b // retry the build

          log.error(s"Failed to start or find started $b")
          context stop self
        }
      }
      // we found a build
      else {
        log.info(s"$b --> ${currentBuilds.head}")
        context stop self
      }
  }
}