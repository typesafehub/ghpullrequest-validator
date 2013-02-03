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
  val SPURIOUS_REBUILD = "SPURIOUS ABORT? -- PLS REBUILD "

  def needsAttention() = {
    val currLabelNames = ghapi.labels(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString).map(_.name)

    if (currLabelNames.contains("tested"))
      ghapi.deleteLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, "tested")

    if (!currLabelNames.contains("needs-attention"))
      ghapi.addLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, List("needs-attention"))
  }

  // TODO: only when all jobs have completed
  def success() = {
    val currLabelNames = ghapi.labels(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString).map(_.name)

    if (!currLabelNames.contains("tested"))
      ghapi.addLabel(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString, List("tested"))
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

      val message =
        status.result match {
          case "FAILURE" =>
            needsAttention()

            import dispatch._
            val consoleOutput = Http(url(status.url) / "consoleText" >- identity[String])
            val (log, failureLog) = consoleOutput.lines.span(! _.startsWith("BUILD FAILED"))

            if (log.exists(_.contains("test.suite"))) {
              log.filter(_.contains("[FAILED]")).map(cleanPartestLine).toList.mkString("\nFailed tests:", "\n", "")
            } else {
              val (a, bs) = failureLog.span(! _.startsWith("Total time: "))
              (a.toList + bs.take(1).toList.headOption.getOrElse("")).mkString("\n")
            }

          case "ABORTED" =>
            needsAttention()

            // if aborted and not rebuilt before, try rebuilding
            val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)

            val comment =
              if (comments.exists(_.body.contains(SPURIOUS_REBUILD))) "Tried automatically rebuilding once before, not falling for it again!"
              else SPURIOUS_REBUILD + job.name

            ghapi.addPRComment(user, repo, pull.number.toString, comment)

            "Build aborted."
          case _ =>
            ""
        }

      ghapi.setCommitStatus(user, repo, sha, CommitStatus.jobEnded(job.name, status.url, status.result == "SUCCESS", message))

      notify ! CommitDone(sha, job)
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}