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

  // TODO: only when all jobs have completed
  def checkSuccess() = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString

    val commits = ghapi.pullrequestcommits(user, repo, pullNum)
    val currLabelNames = ghapi.labels(user, repo, pullNum).map(_.name)
    val hasTestedLabel = currLabelNames.contains("tested")
    val success = commits.forall { c => ghapi.commitStatus(user, repo, c.sha).forall(_.success) }

    if (success && !hasTestedLabel)
      ghapi.addLabel(user, repo, pullNum, List("tested"))
    else if (!success && hasTestedLabel)
      ghapi.deleteLabel(user, repo, pullNum, "tested")
  }


//      // purposefully only at start of line to avoid conditional LGTMs
//      // TODO: do this on every new comment
//      if (comments.exists(_.body.startsWith("LGTM")))
//        ghapi.addLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, List("reviewed")) // TODO: remove when it disappears


  private val user = pull.base.repo.owner.login
  private val repo = pull.base.repo.name


  def receive: Receive = {
    case BuildStarted(url) =>
      ghapi.setCommitStatus(user, repo, sha, CommitStatus.jobStarted(job.name, url))

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

            val jobDesc = "Job "+ job.name +" failed for "+ sha.take(6)

            val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)

            // comment anyway because github only looks at last commit to determine merge status
            // TODO: why do we double-comment?
            if (!comments.exists(_.body.startsWith(jobDesc)))
              ghapi.addPRComment(user, repo, pull.number.toString,
                  jobDesc +":\n"+
                  (if (failedTests.nonEmpty) failedTests.mkString("Failed tests:\n", "\n", "\n") else "\n") +
                  durationReport +
                  "<br> ![sad kitty](http://cdn.memegenerator.net/instances/100x/31464013.jpg)"
                  )

            val testReport = if (failedTests.nonEmpty) (failedTests.length+ " tests failed. ") else ""

            "Build failed."+ testReport + durationReport

          case "ABORTED" =>
            needsAttention()

            // if aborted and not rebuilt before, try rebuilding
            val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)

            val comment =
              if (comments.exists(_.body.contains(SPURIOUS_REBUILD))) "Tried automatically rebuilding once before, not falling for it again!"
              else SPURIOUS_REBUILD +" -- PLS REBUILD "+ job.name

            ghapi.addPRComment(user, repo, pull.number.toString, comment)

            "Build aborted."
          case _ =>
            durationReport
        }

      ghapi.setCommitStatus(user, repo, sha, CommitStatus.jobEnded(job.name, status.url, status.result == "SUCCESS", message))

      checkSuccess()

      notify ! CommitDone(sha, job)
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}