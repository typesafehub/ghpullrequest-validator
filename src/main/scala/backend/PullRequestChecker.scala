package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching
import rest.github.PRCommit
import rest.github.CommitStatus
import rest.github.Pull


case class CheckPullRequests(username: String, project: String)
case class CheckPullRequest(pull: Pull)
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
    case CheckPullRequest(pull) =>
      checkSuccess(pull)
      checkLGTM(pull)
      checkPullRequest(pull)
      // TODO: set milestone
    case CommitDone(pull, sha, job, success) =>
      if(success) checkSuccess(pull)
      else needsAttention(pull)

      active.-=((sha, job))
      forced.-=((sha, job))
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

    // only look at most recent status (head of the stati)
    val success = jenkinsJobs forall (j => commitStati.map(_._2.filter(_.forJob(j.name)).headOption.map(_.success).getOrElse(false)).reduce(_ && _))

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
        if (noop)
          log.debug("Looking for build of "+ sha +" in #"+ pull.number +" job: "+ job)
        else
          log.debug("May build commit "+ sha +" for #"+ pull.number +" job: "+ job)

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

      jenkinsJobs.map { j =>
        j.name +":\n"+ commitStati.map { case (c, sts) =>
          "  - "+ c.sha.take(8) +": "+ sts.filter(_.forJob(j.name)).distinct.mkString(", ")
        }.mkString("\n")
      }.mkString("\n")
    }

    findNewCommands("PLS REBUILD") foreach { case (prefix, body) =>
      val job = body.split("PLS REBUILD").last.lines.take(1).toList.headOption.getOrElse("")
      val trimmed = job.trim
      val (jobs, shas) =
        if (trimmed.startsWith("ALL")) (jenkinsJobs, commits.map(_.sha))
        else if (trimmed.startsWith("/")) trimmed.drop(1).split("@") match { // generated by us for a (spurious) abort
          case Array(job, sha) => (Set(JenkinsJob(job)), List(sha))
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

    commits foreach { c =>
      val stati = ghapi.commitStatus(user, repo, c.sha).filterNot(_.failed)
      jenkinsJobs foreach { j =>
        val jobStati = stati.filter(_.forJob(j.name))
        if (jobStati.isEmpty)
          buildCommit(c.sha, j)
        else if(jobStati.head.pending)
          buildCommit(c.sha, j, noop = true)
      }
    }
  }
}
