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

  def needsAttention() = {
    val currLabelNames = ghapi.labels(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString).map(_.name)

    if (currLabelNames.contains("tested"))
      ghapi.deleteLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, "tested")

    if (!currLabelNames.contains("needs-attention"))
      ghapi.addLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, List("needs-attention"))
  }


//      // purposefully only at start of line to avoid conditional LGTMs
//      // TODO: do this on every new comment
//      if (comments.exists(_.body.startsWith("LGTM")))
//        ghapi.addLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, List("reviewed")) // TODO: remove when it disappears


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
            needsAttention()

            import dispatch._
            val consoleOutput = Http(url(status.url) / "consoleText" >- identity[String])
            val (log, failureLog) = consoleOutput.lines.span(! _.startsWith("BUILD FAILED"))

            val failedTests =
              if (log.exists(_.contains("test.suite"))) {
                log.filter(_.contains("[FAILED]")).map(cleanPartestLine).toList
              } else Nil

            val jobDesc = "Job "+ job.name +" failed for "+ sha.take(8)

            val commitComments = ghapi.commitComments(user, repo, sha)

            // comment anyway because github only looks at last commit to determine merge status
            // TODO: why do we double-comment?
            if (!commitComments.exists(_.body.startsWith(jobDesc)))
              ghapi.addCommitComment(user, repo, sha,
                  jobDesc +" [(results)]("+ status.url +"):\n"+
                  (if (failedTests.nonEmpty) failedTests.mkString("Failed tests:\n", "\n", "\n") else "\n") +
                  "<br>"+ durationReport +
                  "<br> ![sad kitty](http://cdn.memegenerator.net/instances/100x/31464013.jpg)"
                  )

            val testReport = if (failedTests.nonEmpty) (failedTests.length+ " failures. ") else ""

            " failed: "+ testReport + durationReport

          case "ABORTED" =>
            needsAttention()

            // if aborted and not rebuilt before, try rebuilding
            val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)

            if (!comments.exists(_.body.contains(SPURIOUS_REBUILD)))
              ghapi.addPRComment(user, repo, pull.number.toString, SPURIOUS_REBUILD +" -- PLS REBUILD/"+ job.name + "@"+ sha)

            "Build aborted."
          case _ =>
            durationReport
        }

      addStatus(CommitStatus.jobEnded(job.name, status.url, status.result == "SUCCESS", message))

      notify ! CommitDone(pull, sha, job)
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}