package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


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
  
  
  def receive: Receive = {
    case CheckPullRequest(pull, jobs) =>
      checkPullRequest(pull, jobs)
    case CommitDone(sha, job) =>
      active.-=((sha, job))
  }

  // TODO - Ordering.
  private def checkPullRequest(pull: rest.github.Pull, jenkinsJobs: Set[JenkinsJob]): Unit = {
    val user = pull.base.repo.owner.login
    val repo = pull.base.repo.name
    val comments = ghapi.pullrequestcomments(user, repo, pull.number.toString)
    
    val rebuildCommands = comments collect { case c if c.body.contains("PLS REBUILD") =>
      val job = c.body.split("PLS REBUILD").last.lines.take(1).toList.headOption.getOrElse("")
      val trimmed = job.trim
      val jobs =
        if (trimmed.startsWith("ALL")) jenkinsJobs
        else jenkinsJobs.filter(j => trimmed.contains(j.name))

      (jobs, c.id, c.body.replace("PLS REBUILD"+job, ":cat: Roger! Rebuilding "+ jobs.map(_.name).mkString(", ")+ ". :rotating_light:\n"))
    }

    val forcedJobs = rebuildCommands.map(_._1).flatten
    rebuildCommands foreach {case (_, id, newBody) =>
      ghapi.editPRComment(user, repo, id, newBody)
    }
    

    // TODO: use futures
    val commits = ghapi.pullrequestcommits(user, repo, pull.number.toString)

    if (forcedJobs.nonEmpty) {
      commits foreach (c => forcedJobs foreach (j => buildCommit(c.sha, j, force = true)))
    } else {
      commits foreach { c =>
        val stati = ghapi.commitStatus(user, repo, c.sha).filterNot(_.failed)
        jenkinsJobs foreach { j =>
          val jobStati = stati.filter(_.forJob(j.name))
          if (jobStati.isEmpty)
            buildCommit(c.sha, j)
          else if(stati.forall(_.pending))
            buildCommit(c.sha, j) // won't trigger a new build if one is already running for this sha and job
        }
      }
    }

    def buildCommit(sha: String, job: JenkinsJob, force: Boolean = false) = if (force || !active(sha, job)) {
      log.debug("build commit "+ sha +" for #"+ pull.number +" job: "+ job)
      active.+=((sha, job))
      jobBuilder ! BuildCommit(sha, job,
                                Map("pullrequest" -> pull.number.toString,
                                    "sha" -> sha,
                                    "mergebranch" -> pull.base.ref),
                                force,
                                context.actorOf(Props(new PullRequestCommenter(ghapi, pull, job, sha, self)), job.name +"-commenter-"+ sha))
    }
  }
  
}
