package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching
import rest.github.PRCommit
import rest.github.CommitStatus


case class CheckPullRequests(username: String, project: String, jobs: Set[JenkinsJob])
case class CheckPullRequest(pull: rest.github.Pull, jobs: Set[JenkinsJob])
case class CommitDone(sha: String, job: JenkinsJob)


/** This actor is responsible for validating that a pull request has had all required tests executed
 * and that they have passed.
 * 
 * Note: Any job sent to this actor must support one and only one build parameter,
 * "pullrequest"
 */
class PullRequestChecker(ghapi: GithubAPI, jobBuilderProps: Props) extends Actor with ActorLogging{
  val jobBuilder = context.actorOf(jobBuilderProps, "job-builder")
  
  // cache of currently validating commits so we don't duplicate effort....
  val active = collection.mutable.HashSet[(String, JenkinsJob)]()
  val forced = collection.mutable.HashSet[(String, JenkinsJob)]()
  
  
  def receive: Receive = {
    case CheckPullRequest(pull, jobs) =>
      checkPullRequest(pull, jobs)
    case CommitDone(sha, job) =>
      active.-=((sha, job))
      forced.-=((sha, job))
  }

  // TODO - Ordering.
  private def checkPullRequest(pull: rest.github.Pull, jenkinsJobs: Set[JenkinsJob]): Unit = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val pullNum = pull.number.toString
    val comments = ghapi.pullrequestcomments(user, repo, pullNum)
    val IGNORE_NOTE_TO_SELF = "(kitty-note-to-self: ignore "

    // must add a comment that starts with the first element of each returned pair
    def findNewCommands(command: String): List[(String, String)] =
      comments collect { case c
        if c.body.contains(command)
        && !comments.exists(_.body.startsWith(IGNORE_NOTE_TO_SELF+ c.id)) =>

        (IGNORE_NOTE_TO_SELF+ c.id +")\n", c.body)
      }

    val rebuildCommands = findNewCommands("PLS REBUILD") map { case (prefix, body) =>
      val job = body.split("PLS REBUILD").last.lines.take(1).toList.headOption.getOrElse("")
      val trimmed = job.trim
      val jobs =
        if (trimmed.startsWith("ALL")) jenkinsJobs
        else jenkinsJobs.filter(j => trimmed.contains(j.name))

      (jobs, prefix +":cat: Roger! Rebuilding "+ jobs.map(_.name).mkString(", ")+ ". :rotating_light:\n")
    }

    val forcedJobs = rebuildCommands.map(_._1).flatten
    rebuildCommands foreach {case (jobs, newBody) if jobs.nonEmpty =>
      ghapi.addPRComment(user, repo, pullNum, newBody)
    }

    // TODO: use futures
    val commits = ghapi.pullrequestcommits(user, repo, pull.number.toString)

    def buildLog(commits: List[PRCommit]) = {
      val commitStati = commits map (c => (c, ghapi.commitStatus(user, repo, c.sha)))

      jenkinsJobs.map{ j =>
        def describeCS(cs: CommitStatus) =
          (if (cs.url.nonEmpty) "["+cs.state+"]("+ cs.url.get +")" else cs.state)+ cs.description.getOrElse("")

        def describe(jss: (PRCommit, List[CommitStatus])) = "  - "+ jss._1.sha.take(8)+": "+ jss._2.map(describeCS).mkString(", ")

        val jobStati = commitStati.map{case (c, stati) => (c, stati.filter(_.forJob(j.name)))}.map(describe)

        j.name +":\n"+ jobStati.mkString("\n")
      }.mkString("\n\n")
    }

    val buildLogCommands = findNewCommands("BUILDLOG?") map { case (prefix, body) =>
      ghapi.addPRComment(user, repo, pullNum, prefix + buildLog(commits))
    }

    if (forcedJobs.nonEmpty) {
      commits foreach (c => forcedJobs foreach (j => buildCommit(c.sha, j, force = true)))
    } else {
      commits foreach { c =>
        val stati = ghapi.commitStatus(user, repo, c.sha).filterNot(_.failed)
        jenkinsJobs foreach { j =>
          val jobStati = stati.filter(_.forJob(j.name))
          if (jobStati.isEmpty)
            buildCommit(c.sha, j)
          else if(jobStati.last.pending)
            buildCommit(c.sha, j, noop = true)
        }
      }
    }

    def buildCommit(sha: String, job: JenkinsJob, force: Boolean = false, noop: Boolean = false) = if ( (force && !forced(sha, job)) || !active(sha, job)) {
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
  }
  
}
