package backend

import akka.actor.{ActorRef,Actor, Props}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


case class CheckPullRequests(username: String, project: String, jobs: Set[JenkinsJob])
case class CheckPullRequest(pull: rest.github.Pull, jobs: Set[JenkinsJob])
case class CheckPullRequestDone(pull: rest.github.Pull, job: JenkinsJob)


/** This actor is responsible for validating that a pull request has had all required tests executed
 * and that they have passed.
 * 
 * Note: Any job sent to this actor must support one and only one build parameter,
 * "pullrequest"
 */
class PullRequestChecker(ghapi: GithubAPI, jobBuilderProps: Props) extends Actor {
  val jobBuilder = context.actorOf(jobBuilderProps, "job-builder")
  
  // cache of currently validating pull requests so we don't duplicate effort....
  var active = Set.empty[String]
  
  
  def receive: Receive = {
    case CheckPullRequest(pull, jobs)    => checkPullRequest(pull, jobs)
    case CheckPullRequestDone(pull, job) => active -= hash(pull, job)
  }
  
  /** Generates a hash of a pullrequest/job pairing so we don't duplicate work. */
  private def hash(pull: rest.github.Pull, job: JenkinsJob) =
    "%s-%s-%s-%s" format (pull.base.repo.owner.login,
                          pull.base.repo.name,
                          pull.head.sha,
                          job.name)
  
  // TODO - Ordering.
  private def checkPullRequest(pull: rest.github.Pull, jenkinsJobs: Set[JenkinsJob]): Unit = {
    val comments = ghapi.pullrequestcomments(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString)
    val commits = ghapi.pullrequestcommits(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString)
    def lastJobDoneTime(job: JenkinsJob): Option[String] = (
        for {
          comment <- comments
          if comment.body startsWith ("jenkins job " + job)
        } yield comment.created_at
      ).lastOption 
    
    def lastJobRequestTime(job: JenkinsJob): String = {
      val created: Seq[String] = Vector(pull.created_at)
      val requests = for {
        comment <- comments
        if (comment.body contains ("PLS REBUILD ALL")) || (comment.body contains ("PLS REBUILD " + job))
      } yield comment.created_at
      // TODO - Check commit times, so we rebuild on new commits.
      // val newCommits = <search commits for last updated time>
      val commitTimes = commits map (_.commit.author.date)
      (created ++ requests ++ commitTimes) reduce ( (x,y) => if (x > y) x else y )
    }
    
    def needsRebuilt(job: JenkinsJob): Boolean =
      (lastJobDoneTime(job) 
          map (_ < lastJobRequestTime(job)) 
          getOrElse true)
          
    val builds = jenkinsJobs filter needsRebuilt

    def makeCommenter(job: JenkinsJob): ActorRef =
      context.actorOf(Props(new PullRequestCommenter(ghapi, pull, job, self)), job + "-commenter-" + pull.number)
    
    // For all remaining verification jobs, spit out a new job.
    for {
      job <- (builds).toSeq.sorted
      if !active(hash(pull, job))
    } {
      active += hash(pull,job)
      jobBuilder ! BuildProject(job, 
                                Map("pullrequest" -> pull.number.toString,
                                    "mergebranch" -> pull.base.ref), 
                                makeCommenter(job))
    }
  }
  
}
