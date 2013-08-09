package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging, ReceiveTimeout}
import scala.concurrent.duration._
import rest.github.{API=>GithubAPI, PRCommit, CommitStatus, Pull, PullMini, Milestone}

case class CheckPullRequests(username: String, project: String)
case class CheckPullRequest(username: String, project: String, pull: PullMini, branchToMS: Map[String, rest.github.Milestone], poller: ActorRef)
case class PullRequestChecked(username: String, project: String, pull: PullMini)
case class CommitDone(pull: Pull, sha: String, job: JenkinsJob, success: Boolean)


/** This actor is responsible for validating that a pull request has had all required tests executed
 * and that they have passed.
 * 
 * Note: Any job sent to this actor must support one and only one build parameter,
 * "pullrequest"
 */
class PullRequestChecker(ghapi: GithubAPI, jenkinsJobs: Set[JenkinsJob], jobBuilderProps: Props) extends Actor with ActorLogging{
  val jobBuilder = context.actorOf(jobBuilderProps, "job-builder")

  // cache of currently validating commits so we don't duplicate effort....
  val pending = collection.mutable.HashSet[(String, JenkinsJob)]()
  context setReceiveTimeout (5 minutes)

  def receive: Receive = {
    // in case something went wrong, forget about supposedly pending builds every 5 minutes
    // do still guard against starting them multiple times in close succession
    case ReceiveTimeout =>
      pending.clear()

    case CheckPullRequest(user, proj, pullMini, branchToMS, poller) =>
      try {
        val pull = ghapi.pullrequest(user, proj, pullMini.number)
        // TODO: make configurable whether to ignore PRs that don't have a corresponding milestone (used in scala for ignoring PRs to gh-pages)
        if (proj != "scala" || (branchToMS isDefinedAt pull.base.ref)) {
          checkMilestone(pull, branchToMS)
          checkLGTM(pull)
          checkPullRequest(pull)
          checkSuccess(pull)
          poller ! PullRequestChecked(user, proj, pullMini)
        }
      } catch {
        case x@(_ : dispatch.classic.StatusCode | _ : java.net.SocketTimeoutException) =>
          log.error(s"Problem while checking $user/$proj#${pullMini.number}\n$x")
          throw x
      }
    case CommitDone(pull, sha, job, success) =>
      try {
        if(success) checkSuccess(pull)
        else needsAttention(pull)

        pending.-=((sha, job))
      } catch {
        case x@(_ : dispatch.classic.StatusCode | _ : java.net.SocketTimeoutException) =>
          log.error(s"Problem while marking $pull as done\n$x")
          throw x
      }
  }

  // if there's a milestone with description "Merge to ${pull.base.ref}.", set it as the PR's milestone
  private def checkMilestone(pull: Pull, branchToMS: Map[String, rest.github.Milestone]) = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString

    branchToMS get(pull.base.ref) foreach { milestone =>
      val msNum = milestone.number
      ghapi.issue(user, repo, pullNum).milestone match {
        case Some(Milestone(_ /*`ghNum`*/, _, _)) => // allow any milestone, don't overwrite
        case _ =>
          log.debug(s"Setting milestone of $pull to ${milestone.title}")
          ghapi.setMilestone(user, repo, pullNum, msNum)
      }

    }
  }

  private def checkLGTM(pull: Pull) = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString
    val comments = ghapi.pullrequestcomments(user, repo, pullNum)
    val currLabelNames = ghapi.labels(user, repo, pullNum).map(_.name)
    val reviewLabel = currLabelNames.contains("reviewed")

    // purposefully only at start of line to avoid conditional LGTMs
    val LGTMed = comments.exists(_.body.startsWith("LGTM"))

    if (!reviewLabel && LGTMed)
      ghapi.addLabel(user, repo, pullNum, List("reviewed"))
    else if(reviewLabel && !LGTMed)
      ghapi.deleteLabel(user, repo, pullNum, "reviewed")
  }

  private def needsAttention(pull: Pull) = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString
    val currLabelNames = ghapi.labels(user, repo, pullNum).map(_.name)

    if (currLabelNames.contains("tested"))
      ghapi.deleteLabel(user, repo, pullNum, "tested")

    if (!currLabelNames.contains("needs-attention"))
      ghapi.addLabel(user, repo, pullNum, List("needs-attention"))
  }

  private def checkSuccess(pull: Pull) = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString

    val commits = ghapi.pullrequestcommits(user, repo, pullNum)
    val currLabelNames = ghapi.labels(user, repo, pullNum).map(_.name)
    val hasTestedLabel = currLabelNames.contains("tested")

    val commitStati = commits map (c => (c.sha, ghapi.commitStatus(user, repo, c.sha)))

    // only look at most recent status (head of the stati) -- if it's either success
    // .... or done (in principle it shouldn't happen that there's a mix of only _.done and _.success,
    // but due to a bug that's now fixed some stati are messed up like that, and if all jobs are done || success, it's ok for sure)
    val success = jenkinsJobs forall (j => commitStati.map{case (_, cs) => CommitStatus.jobDoneOk(cs.filter(_.forJob(j.name)))}.reduce(_ && _))

    if (!success)
      log.debug(s"checkSuccess failed for $pull -->" +
          commitStati.map{case (sha, sts) => sha.take(6) +": "+ sts.distinct.mkString(", ") }.mkString("\n")
        )

    if (success && !hasTestedLabel) {
      ghapi.addLabel(user, repo, pullNum, List("tested"))

      if (currLabelNames.contains("needs-attention"))
        ghapi.deleteLabel(user, repo, pullNum, "needs-attention")
    }
    else if (!success && hasTestedLabel)
      ghapi.deleteLabel(user, repo, pullNum, "tested")
  }

  // TODO - Ordering.
  private def checkPullRequest(pull: Pull): Unit = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString
    val IGNORE_NOTE_TO_SELF = "(kitty-note-to-self: ignore "

    // synch when run during the first 10 minutes of every other hour
    def randomlySynch(): Boolean = {
      val now = new java.util.Date
      (now.getHours % 2 == 0) && (now.getMinutes <= 10)
    }
    def synch(commits: List[PRCommit]): Unit = {
      commits foreach { c =>
        jenkinsJobs foreach { j =>
          buildCommit(c.sha, j, noop = true)
        }
      }
    }

    // does not start another job when we have an pending job (when !force),
    def buildCommit(sha: String, job: JenkinsJob, force: Boolean = false, noop: Boolean = false) =
      if (noop || !pending((sha, job))) {
        if (!noop) pending.+=((sha, job))
        // TODO: find actor by name? for now not specifying a name as it's no longer unique for the longer-running commenter
        // val forcedUniq = if (force) "-forced" else ""
        // val name = job.name +"-commenter-"+ sha + forcedUniq
        val commenter = context.actorOf(Props(new PullRequestCommenter(ghapi, pull, job, sha, self)))

        jobBuilder ! BuildCommit(sha, job,
                                  Map("pullrequest" -> pull.number.toString,
                                      "sha" -> sha,
                                      "mergebranch" -> pull.base.ref),
                                  force,
                                  noop,
                                  commenter)
      } else log.warning(s"Not building $job for $pull@${sha.take(8)} (force: $force, noop: $noop, pending: ${pending((sha, job))}).")

    val comments = ghapi.pullrequestcomments(user, repo, pullNum)

    // must add a comment that starts with the first element of each returned pair
    def findNewCommands(command: String): List[(String, String)] =
      comments collect { case c
        if c.body.contains(command)
        && !comments.exists(_.body.startsWith(IGNORE_NOTE_TO_SELF+ c.id)) =>

        (IGNORE_NOTE_TO_SELF+ c.id +")\n", c.body)
      }

    def buildLog(commits: List[PRCommit]) = {
      val commitStati = commits map (c => (c, ghapi.commitStatus(user, repo, c.sha)))
      def shortString(job: String)(cs: CommitStatus) = cs.stateString +":"+ cs.description.getOrElse("").replace(job, "")
      jenkinsJobs.map { j =>
        j.name +":\n"+ commitStati.map { case (c, sts) =>
          "  - "+ c.sha.take(8) +": "+ sts.filter(_.forJob(j.name)).distinct.map(shortString(j.name)).mkString(", ")
        }.mkString("\n")+"\n"
      }.mkString("\n")
    }

    // TODO: use futures
    val commits = ghapi.pullrequestcommits(user, repo, pull.number.toString)

    findNewCommands("PLS REBUILD") foreach { case (prefix, body) =>
      val job = body.split("PLS REBUILD").last.lines.take(1).toList.headOption.getOrElse("")
      val trimmed = job.trim
      val (jobs, shas) =
        if (trimmed.startsWith("ALL")) (jenkinsJobs, commits.map(_.sha))
        else if (trimmed.startsWith("/")) trimmed.drop(1).split("@") match { // generated by us for a (spurious) abort
          case Array(job, sha) => (Set(JenkinsJob(job)), List(ghapi.normalizeSha(user, repo, sha)))
          case _ => (Set[JenkinsJob](), Nil)
        }
        else (jenkinsJobs.filter(j => trimmed.contains(j.name)), commits.map(_.sha))

      ghapi.addPRComment(user, repo, pullNum,
          prefix +":cat: Roger! Rebuilding "+ jobs.map(_.name).mkString(", ") +" for "+ shas.map(_.take(8)).mkString(", ") +". :rotating_light:\n")

      shas foreach { sha => jobs foreach { j => 
        pending.-=((sha, j)) // since we're forcing, ignore it was pending
        buildCommit(sha, j, force = true)
      }}
    }

    findNewCommands("BUILDLOG?") map { case (prefix, body) =>
      ghapi.addPRComment(user, repo, pullNum, prefix + buildLog(commits))
    }

    if (randomlySynch()) synch(commits)
    else
      findNewCommands("PLS SYNCH") map { case (prefix, body) =>
        ghapi.addPRComment(user, repo, pullNum, prefix + ":cat: Synchronaising! :pray:")
        synch(commits)
      }

    // delete all commit comments -- don't delete PR comments as they would re-trigger
    // the commands that caused them originally
    findNewCommands("NOLITTER!") map { case (prefix, body) =>
//      comments foreach { comm =>
//        if (comm.user.login == user && !comm.body.startsWith(IGNORE_NOTE_TO_SELF)) {
//          log.debug("Deleting PR comment "+ comm.id)
//          ghapi.deletePRComment(user, repo, comm.id)
//        }
//        else log.debug("Leaving PR comment "+ (comm.id, comm.user.login))
//      }
      ghapi.addPRComment(user, repo, pullNum, prefix + ":cat: cleaning up... sorry! :cat:")
      commits foreach { c =>
        ghapi.commitComments(user, repo, c.sha) foreach { comm =>
          if (comm.user.login == ghapi.userName) {
            log.debug("Deleting commit comment "+ comm.id)
            ghapi.deleteCommitComment(user, repo, comm.id)
          }
          else log.debug("Leaving commit comment "+ (comm.id, comm.user.login))
        }
      }
    }

    log.info(s"Checking $pull (${commits.length} commits).")

    commits foreach { c =>
      val stati = ghapi.commitStatus(user, repo, c.sha)
      jenkinsJobs foreach { j =>
        // if there's a status, assume there's a job(start)watcher
        val jobStati = stati.filter(_.forJob(j.name))
        if (jobStati.nonEmpty) log.info(s"Status of $j for $pull@${c.sha take 4}: ${jobStati.head}")
        else buildCommit(c.sha, j)
      }
    }
  }
}
