package backend

import akka.actor.{ Actor, ActorRef, ActorSystem, Props, ActorLogging, ReceiveTimeout }
import rest.jenkins.{ API => JenkinsAPI }
import akka.util.duration._
import rest.jenkins.BuildStatus

/** A class that watches for a job to begin on jenkins and then
 *  notifies the jenkinsService that the job has started.
 */
// TODO - We need a new way to detect a job has started...
class JenkinsJobStartWatcher(api: JenkinsAPI, b: BuildCommit, jenkinsService: ActorRef) extends Actor with ActorLogging {
  // TODO - Use better time library....

  // Set timeout to check when the job has started.
  context setReceiveTimeout (30 seconds)

  private[this] var retryCount = 0
  private val MAX_RETRIES = 10

  sealed abstract class JenkinsStatus
  case object JQueue extends JenkinsStatus
  case object JRetry extends JenkinsStatus
  case object JStop extends JenkinsStatus
  def jenkinsStatus: JenkinsStatus =
    api.jobInfo(b.job).queueItem match {
      // there's a queue, be patient as jenkins doesn't tell us about queued jobs
      case Some(rest.jenkins.QueueItem(true)) =>
        log.debug("Queue " + b.job + " #" + b.args("pullrequest") + " commit " + b.args("sha"))
        JQueue
      // no queue, if don't start finding it by now, something went wrong
      case _ =>
        log.debug("No queue " + b.job + " #" + b.args("pullrequest") + " commit " + b.args("sha"))
        if (retryCount < MAX_RETRIES) JRetry else JStop
    }

  private val knownRunning = collection.mutable.HashSet[String]()
  private var notifiedCommenter = false // did we add a dummy BuildQueued?
  // we only try to start a build once, if that fails, we'll time out and try again
  private var startedBuild = false

  def receive: Receive = {
    case ReceiveTimeout =>
      jenkinsStatus match {
        case JStop =>
          log.debug("Timed out finding build " + (b.job, b.args.get("pullrequest"), api.jobInfo(b.job)))
          jenkinsService ! b // tell the service to rebuild
          context stop self // stop looking for the job to start

        case s @ (JQueue | JRetry) =>
          // be patient when there's a queue, don't even start a job as we won't be able to tell whether it started
          // if we fire off a build, don't realize it started, and reboot the kitten, it will start a redundant job next time,
          // if the originally started job is still in the queue (as the job is hidden there)
          if (s == JQueue) context setReceiveTimeout (10 minutes)
          else {
            retryCount += 1
            context setReceiveTimeout (1 minutes)
          }

          // we haven't started our own build yet, but maybe an older one is running
          // watch its result, but still start our own
          val relevantBuilds = ourJobs.filter(_.building) match {
            case bs if bs.isEmpty =>
              log.debug("No building jobs for " + b.job + " #" + b.args("pullrequest") + " commit " + b.args("sha") + " all jobs: " + ourJobs)
              ourJobs.headOption.toSet // if no running builds, just look at the most recent one
            case bs => bs.toSet
          }

          val newlyDiscovered = relevantBuilds.filterNot(bs => knownRunning(bs.url))

          val (building, done) = newlyDiscovered.partition(_.building)
          building foreach { status =>
            jenkinsService ! JobStarted(b, status)
            b.commenter ! BuildStarted(status.url)
          }

          // done after sending BuildStarted so that (hopefully) this status is more recent
          done foreach { status =>
            b.commenter ! BuildResult(status)
          }

          knownRunning ++= newlyDiscovered.map(_.url)

          val canStop = startedBuild || !b.force
          val foundJob = newlyDiscovered.nonEmpty
          if (canStop && foundJob) context stop self
          else if (s != JQueue && !b.noop && !startedBuild) {
            startedBuild = true
            api.buildJob(b.job, b.args)
            log.debug("Started job for #" + b.args("pullrequest") + " --> " + b.job.name + " sha: " + b.args("sha"))
          }

          // send BuildQueued no more than once
          if (!notifiedCommenter) {
            notifiedCommenter = true // either because newlyDiscovered.nonEmpty and thus we notified above, or we will now
            if (newlyDiscovered.isEmpty)
              b.commenter ! BuildQueued
          }

      }
  }

  def ourJobs =
    api.buildStatusForJob(b.job).filter { status =>
      status.actions.parameters.collect { case rest.jenkins.Param(n, v) if b.args.isDefinedAt(n) => (n, v) }.toMap == b.args
    }

}