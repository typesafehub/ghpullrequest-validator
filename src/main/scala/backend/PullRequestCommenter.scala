package backend

import akka.actor.{ActorRef, Actor, Props, ActorLogging}
import rest.github.{API=>GithubAPI}
import rest.github.CommitStatus


/** Comments on pull request `pull` with the result of the jenkins job `job` that's building commit `sha`.
 *
 * One actor per unique combination of pull/job/sha/forced (forced == triggered by PLS REBUILD command)
 *
 * Note: This only helps the PulLRequestValidator actors and should not be used stand-alone.
 */
class PullRequestCommenter(ghapi: GithubAPI, pull: rest.github.Pull, job: JenkinsJob, shaMaybeShort: String, notify: ActorRef) extends Actor with ActorLogging {
  val SPURIOUS_REBUILD = "SPURIOUS ABORT?"

  private val user = pull.base.repo.owner.login
  private val repo = pull.base.repo.name

  lazy val sha = ghapi.normalizeSha(user, repo, shaMaybeShort)

  def addStatus(status: CommitStatus) = {
    if (!ghapi.commitStatus(user, repo, sha).take(1).contains(status)) {
      log.info(s"Set status for $job on $pull@${sha take 6}: $status.")
      Thread sleep (scala.util.Random.nextFloat * 5000).toInt // randomly sleep up to 5s to avoid setting multiple statuses with exactly the same timestamp
      ghapi.setCommitStatus(user, repo, sha, status)
    } else log.info(s"DROPPED status for $job on $pull@${sha take 6}: $status.")
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

      val message =
        status.result match {
          case "FAILURE" =>
            import dispatch._
            val consoleOutput = rest.Http(dispatch.classic.url(status.url) / "consoleText" >- identity[String])
            val (logLines, failureLog) = consoleOutput.lines.span(! _.startsWith("BUILD FAILED"))

            val failedTests =
              if (logLines.exists(_.contains("test.suite"))) {
                logLines.filter(_.contains("[FAILED]")).map(cleanPartestLine).toList
              } else Nil

            val jobDesc = "Job "+ job.name +" failed for "+ sha.take(8)

            val message = jobDesc +" [(results)]("+ status.url +"):\n"+
              (if (failedTests.nonEmpty) failedTests.mkString("Failed tests:\n", "\n", "\n") else "\n") +
              "<br>"+ status.friendlyDuration +
              """<br> to rebuild, comment "PLS REBUILD/"""+ job.name + "@"+ sha+ """" on PR """+ pull.number // not referencing the PR github-style as that causes lots of noise

            log.debug("Failed: "+ message)

            try {
              val commitComments = ghapi.commitComments(user, repo, sha)

              // comment anyway because github only looks at last commit to determine merge status
              // TODO: why do we double-comment?
              if (!commitComments.exists(_.body.startsWith(jobDesc)))
                ghapi.addCommitComment(user, repo, sha, message)
            } catch {
              case s: dispatch.classic.StatusCode =>
                log.error("HTTP error "+ s)
            }

            val testReport = if (failedTests.nonEmpty) (failedTests.length+ " failures. ") else ""

            " failed: "+ testReport + status.friendlyDuration

          case "ABORTED" =>
            // if aborted and not rebuilt before, try rebuilding
            val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)

            if (!comments.exists(_.body.contains(SPURIOUS_REBUILD)))
              ghapi.addPRComment(user, repo, pull.number.toString, SPURIOUS_REBUILD +" -- PLS REBUILD/"+ job.name + "@"+ sha)

            "Build aborted."
          case _ =>
            status.friendlyDuration
        }

      val ok = status.result == "SUCCESS"
      val myStatus = CommitStatus.jobEnded(job.name, status.url, ok, message)
      addStatus(myStatus)
      notify ! CommitDone(pull, sha, job, ok)

      // We want to ensure that a PR is only ok if all commits and all jobs are ok.
      // Unfortunately, GitHub only looks at the most recent status of the last commit of the PR to determine the status of the whole PR.
      // As we can only prepend new statuses to a commit while having to support multiple jobs per commit and multiple commits per PR,
      // we implement the following strategy to implement the desired behaviour.
      // For every commit, we copy error/pending jobs that have no corresponding success to the head of the list.
      // For the last commit, we add a fake pending status if there are earlier commits that meet the above condition
      //   (having an error/pending status without corresponding success).
      if (ok) {
        val commits      = ghapi.pullrequestcommits(user, repo, pull.number.toString)
        val priorCommits = if (commits.lengthCompare(1) > 0 && commits.last.sha == sha) commits.init else Nil
        val overrides    = CommitStatus.overruleSuccess(ghapi, user, repo, sha, job.name, status.url, message, priorCommits)
        overrides foreach addStatus

        // have to stay alive regardless as the job start watcher may call at any time for a PLS SYNCH or a PLS REBUILD ALL
        // if (overrides.isEmpty) {
        //   log.info(s"Stopping commenter for $job of $pull.")
        //   context stop self // can only stop on success -- if we got a failure result, we may still get a later success
        // }
      }
  }
}
