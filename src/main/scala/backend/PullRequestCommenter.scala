package backend

import akka.actor.{ActorRef, Actor, Props, ActorLogging}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching
import rest.github.CommitStatus


/** Comments on pull request `pull` with the result of the jenkins job `job` that's building commit `sha`.
 * Note: This only helps the PulLRequestValidator actors and should not be used
 * stand-alone.
 */
class PullRequestCommenter(ghapi: GithubAPI, pull: rest.github.Pull, job: JenkinsJob, sha: String, notify: ActorRef) extends Actor with ActorLogging {
  val SPURIOUS_REBUILD = "SPURIOUS ABORT?"

  private val user = pull.base.repo.owner.login
  private val repo = pull.base.repo.name

  def addStatus(status: CommitStatus) = {
    if (!ghapi.commitStatus(user, repo, sha).contains(status)) {
      log.debug("Status " + status.state + " set for " + job.name + ", #" + pull.number + " on " + sha.take(8) + " at " + status.target_url.getOrElse(""))

      ghapi.setCommitStatus(user, repo, sha, status)
    } else
      log.debug("Already set status " + status.state + " for " + job.name + ", #" + pull.number + " on " + sha.take(8) + " at " + status.target_url.getOrElse(""))
  }

  def receive: Receive = {
    case BuildQueued =>
      addStatus(CommitStatus.jobQueued(job.name))

    case BuildStarted(url) =>
      addStatus(CommitStatus.jobStarted(job.name, url))

    case BuildResult(status) =>
      def cleanPartestLine(line: String) = ("  - " + {
        try { line.split("/files/")(1).split("\\[FAILED\\]").head.trim }
        catch {
          case _: Exception => line
        }})

      val duration = try { status.duration.toInt / 1000 } catch { case x: Exception => 0 }
      val durationReport = "Took " + (if (duration < 120) duration + " s." else (duration / 60) + " min.")

      val message =
        status.result match {
          case "FAILURE" =>
            import dispatch._
            val consoleOutput = Http(url(status.url) / "consoleText" >- identity[String])
            val (logLines, failureLog) = consoleOutput.lines.span(! _.startsWith("BUILD FAILED"))

            val failedTests =
              if (logLines.exists(_.contains("test.suite"))) {
                logLines.filter(_.contains("[FAILED]")).map(cleanPartestLine).toList
              } else Nil

            val jobDesc = "Job "+ job.name +" failed for "+ sha.take(8)

            val message = jobDesc +" of #"+ pull.number +" [(results)]("+ status.url +"):\n"+
              (if (failedTests.nonEmpty) failedTests.mkString("Failed tests:\n", "\n", "\n") else "\n") +
              "<br>"+ durationReport +
              "<br> ![sad kitty](http://cdn.memegenerator.net/instances/100x/31464013.jpg)"

            log.debug("Failed: "+ message)

            try {
              val commitComments = ghapi.commitComments(user, repo, sha)

              // comment anyway because github only looks at last commit to determine merge status
              // TODO: why do we double-comment?
              if (!commitComments.exists(_.body.startsWith(jobDesc)))
                ghapi.addCommitComment(user, repo, sha, message)
            } catch {
              case s: dispatch.StatusCode =>
                log.debug("HTTP error "+ s)
            }

            val testReport = if (failedTests.nonEmpty) (failedTests.length+ " failures. ") else ""

            " failed: "+ testReport + durationReport

          case "ABORTED" =>
            // if aborted and not rebuilt before, try rebuilding
            val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)

            if (!comments.exists(_.body.contains(SPURIOUS_REBUILD)))
              ghapi.addPRComment(user, repo, pull.number.toString, SPURIOUS_REBUILD +" -- PLS REBUILD/"+ job.name + "@"+ sha)

            "Build aborted."
          case _ =>
            durationReport
        }

      val ok = status.result == "SUCCESS"
      addStatus(CommitStatus.jobEnded(job.name, status.url, ok, message))

      // avoid false positives when a faster job completes successfully before a longer-running job has a chance to indicate failure
      // by prepending the pending status of other jobs that have a pending status but no final result
      if (ok) {
        val otherPendingStati = ghapi.commitStatus(user, repo, sha).filterNot(st => st.forJob(job.name))
        otherPendingStati.groupBy(_.job.getOrElse("")) foreach { case (job, stati) =>
          if (stati.nonEmpty && stati.forall(st => st.pending || st.failed))
            ghapi.setCommitStatus(user, repo, sha, stati.head)
        }
      }

      notify ! CommitDone(pull, sha, job, status.result == "SUCCESS")
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}