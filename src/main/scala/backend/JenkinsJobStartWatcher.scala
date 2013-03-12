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
  // Set timeout to check when the job has started.
  context setReceiveTimeout (1 minute)

  private val seenBuilds = collection.mutable.HashSet[String]()
  // we only try to start a build once, if that fails, we'll time out and try again
  private var startedBuild = false

  def receive: Receive = {
    case ReceiveTimeout =>
      // jobs for this PR, sha, mergebranch (all params must match)
      val ourJobs = api.buildStatusForJob(b.job).filter { status =>
        val paramsForOurArgs = status.actions.parameters.collect {
          case rest.jenkins.Param(n, v) if b.args.isDefinedAt(n) => (n, v)
        }.toMap

        paramsForOurArgs == b.args
      }

      // where's our build? if there are building jobs, look at those
      // if not, shift our gaze to queued or finished jobs
      val relevantBuilds = ourJobs.filter(_.building) match {
        case bs if bs.isEmpty =>
          log.debug("No building jobs for " + b.job + " #" + b.args("pullrequest") + " commit " + b.args("sha") + " all jobs: " + ourJobs)
          ourJobs.headOption.toSet // if no running builds, just look at the most recent one
        case bs => bs.toSet
      }

      val newlyDiscovered = ourJobs.filterNot(bs => seenBuilds(bs.url))

      val (buildingOrQueued, done) = newlyDiscovered.partition(st => st.building || st.queued)

      buildingOrQueued foreach { status =>
        if (status.queued)
          b.commenter ! BuildQueued
        else {
          jenkinsService ! JobStarted(b, status)
          b.commenter ! BuildStarted(status.url)
        }
      }

      // done after sending BuildStarted so that (hopefully) this status is more recent
      done foreach { status =>
        b.commenter ! BuildResult(status)
      }

      seenBuilds ++= newlyDiscovered.map(_.url)

      val canStop = startedBuild || !b.force
      def foundRunningJob = newlyDiscovered.exists(!_.queued)
      def isQueued = newlyDiscovered.exists(_.queued)

      if (canStop && foundRunningJob) context stop self
      else if (!b.noop && !startedBuild && !isQueued) {
        startedBuild = true
        api.buildJob(b.job, b.args)
        log.debug("Started job for #" + b.args("pullrequest") + " --> " + b.job.name + " sha: " + b.args("sha"))
      }
  }

}