package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging}
import scala.concurrent.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching
import rest.github.PRCommit
import rest.github.CommitStatus
import rest.github.Pull
import rest.github.Milestone


case class CheckPullRequests(username: String, project: String)
case class CheckPullRequest(pull: Pull, branchToMS: Map[String, rest.github.Milestone])
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
  val active = collection.mutable.HashSet[(String, JenkinsJob)]()
  val forced = collection.mutable.HashSet[(String, JenkinsJob)]()
  
  
  def receive: Receive = {
    case CheckPullRequest(pull, branchToMS) =>
      try {
        checkMilestone(pull, branchToMS)
        checkLGTM(pull)
        checkPullRequest(pull)
        checkSuccess(pull)
      } catch {
        case x@(_ : dispatch.classic.StatusCode | _ : java.net.SocketTimeoutException) =>
          log.error(s"Problem while checking ${pull.base.repo.name}#${pull.number} ($pull)\n$x")
          throw x
      }
    case CommitDone(pull, sha, job, success) =>
      try {
        if(success) checkSuccess(pull)
        else needsAttention(pull)

        active.-=((sha, job))
        forced.-=((sha, job))
      } catch {
        case x@(_ : dispatch.classic.StatusCode | _ : java.net.SocketTimeoutException) =>
          log.error(s"Problem while marking ${pull.base.repo.name}#${pull.number} as done ($pull)\n$x")
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
          log.debug("Setting milestone of #"+ pullNum +" to "+ milestone.title)
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
      log.debug("checkSuccess failed for #"+ pull.number +" --> "+ commitStati.map{case (sha, sts) => sha.take(8) +": "+ sts.distinct.mkString(", ") }.mkString("\n"))

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

    // does not start another job when we have an active job (when !forced),
    // forced jobs are also only started once, but tracked separately from active jobs
    def buildCommit(sha: String, job: JenkinsJob, force: Boolean = false, noop: Boolean = false) =
      if ( (force && !forced(sha, job)) || !active(sha, job)) {
        active.+=((sha, job))
        forced.+=((sha, job))
        val forcedUniq = if (force) "-forced" else ""
        jobBuilder ! BuildCommit(sha, job,
                                  Map("pullrequest" -> pull.number.toString,
                                      "sha" -> sha,
                                      "mergebranch" -> pull.base.ref),
                                  force,
                                  noop,
                                  context.actorOf(Props(new PullRequestCommenter(ghapi, pull, job, sha, self)), job.name +"-commenter-"+ sha + forcedUniq))
      }

    val comments = ghapi.pullrequestcomments(user, repo, pullNum)

    // must add a comment that starts with the first element of each returned pair
    def findNewCommands(command: String): List[(String, String)] =
      comments collect { case c
        if c.body.contains(command)
        && !comments.exists(_.body.startsWith(IGNORE_NOTE_TO_SELF+ c.id)) =>

        (IGNORE_NOTE_TO_SELF+ c.id +")\n", c.body)
      }

    // TODO: use futures
    val commits = ghapi.pullrequestcommits(user, repo, pull.number.toString)

    def buildLog(commits: List[PRCommit]) = {
      val commitStati = commits map (c => (c, ghapi.commitStatus(user, repo, c.sha)))
      def shortString(job: String)(cs: CommitStatus) = cs.stateString +":"+ cs.description.getOrElse("").replace(job, "")
      jenkinsJobs.map { j =>
        j.name +":\n"+ commitStati.map { case (c, sts) =>
          "  - "+ c.sha.take(8) +": "+ sts.filter(_.forJob(j.name)).distinct.map(shortString(j.name)).mkString(", ")
        }.mkString("\n")+"\n"
      }.mkString("\n")
    }

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

      shas foreach (sha => jobs foreach (j => buildCommit(sha, j, force = true)))
    }

    findNewCommands("BUILDLOG?") map { case (prefix, body) =>
      ghapi.addPRComment(user, repo, pullNum, prefix + buildLog(commits))
    }

    findNewCommands("PLS SYNCH") map { case (prefix, body) =>
      ghapi.addPRComment(user, repo, pullNum, prefix + ":cat: Synchronaising! :pray:")
      commits foreach { c =>
        val stati = ghapi.commitStatus(user, repo, c.sha).filterNot(_.failed)
        jenkinsJobs foreach { j =>
          val jobStati = stati.filter(_.forJob(j.name))
          buildCommit(c.sha, j, noop = true)
        }
      }
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

    commits foreach { c =>
      val stati = ghapi.commitStatus(user, repo, c.sha)
      jenkinsJobs foreach { j =>
        // if there's no status, or it's failure or pending (it is/was either in our local queue or started on jenkins),
        // we may end up starting a new job if it wasn't found (because the kitten was rebooted before it could move the job from our queue to jenkins)
        // note: forced jobs (PLS REBUILD) have been started above and added to active set, so we won't start them twice
        if (!stati.filter(_.forJob(j.name)).headOption.exists(st => st.success || st.error))
          buildCommit(c.sha, j)
      }
    }
  }
}
