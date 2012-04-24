package backend

import akka.actor.{ActorRef,Actor, Props}
import akka.util.duration._
import rest.github.{API=>GithubAPI}


case class CheckPullRequests(username: String, project: String, jobs: Set[String])
case class CheckPullRequest(pull: rest.github.Pull, jobs: Set[String])
case class CheckPullRequestDone(pull: rest.github.Pull, job: String)

/** A class that continually polls github for pull requests and notifies
 * a listener when they are discovered.
 */
class GhPullPoller(ghapi: GithubAPI, listener: ActorRef) extends Actor {
  def receive: Receive = {
    case c @ CheckPullRequests(user, proj, jobs) => 
      checkPullRequests(user, proj, jobs)
  }
  
  private def checkPullRequests(ghuser: String, ghproject: String, jobs: Set[String]): Unit = 
    // TODO - cull pull requests that haven't changed since the last time we checked....
    for {
      p <- ghapi.pullrequests(ghuser, ghproject)
      pull = ghapi.pullrequest(ghuser, ghproject, p.number.toString)
    } listener ! CheckPullRequest(pull, jobs)
}

/** This actor is responsible for validating that a pull request has had all required tests executed
 * and that they have passed.
 * 
 * Note: Any job sent to this actor must support one and only one build parameter,
 * "pullrequest"
 */
class PullRequestChecker(ghapi: GithubAPI, jobBuilder: ActorRef) extends Actor {
  
  // cache of currently validating pull requests so we don't duplicate effort....
  @volatile
  private var active = Set.empty[String]
  
  
  def receive: Receive = {
    case CheckPullRequest(pull, jobs)    => checkPullRequest(pull, jobs)
    case CheckPullRequestDone(pull, job) => active -= hash(pull, job)
  }
  
  /** Generates a hash of a pullrequest/job pairing so we don't duplicate work. */
  private def hash(pull: rest.github.Pull, job: String) =
    "%s-%s-%s-%s" format (pull.base.repo.owner.login,
                          pull.base.repo.name,
                          pull.head.sha,
                          job)
  
  private def checkPullRequest(pull: rest.github.Pull, jenkinsJobs: Set[String]): Unit = {
    val checkedBuilds = for {
      comment <- ghapi.pullrequestcomments(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString)
      if comment.updated_at > pull.updated_at
      job <- jenkinsJobs
      if comment.body startsWith ("jenkins job " + job)
    } yield job

    def makeCommenter(job: String): ActorRef =
      context.system.actorOf(Props().withCreator(new PullRequestCommenter(ghapi, pull, job, self)))
    
    // For all remaining verification jobs, spit out a new job.
    for {
      job <- (jenkinsJobs -- checkedBuilds).toSeq.sorted
      if !active(hash(pull, job))
    } {
      active += hash(pull,job)
      jobBuilder ! BuildProject(job, 
                                Map("pullrequest" -> pull.number.toString), 
                                makeCommenter(job))
    }
  }
}

/** Comments on a given pull request with the result of a jenkins job. 
 * Note: This only helps the PulLRequestValidator actors and should not be used
 * stand-alone.
 */
class PullRequestCommenter(ghapi: GithubAPI, pull: rest.github.Pull, job: String, notify: ActorRef) extends Actor {
  def receive: Receive = {
    case BuildResult(success, url) =>
      val successString = if(success) "Success" else "Failed"
      val comment = 
        "jenkins job %s: %s - %s" format (job, successString, url)
      ghapi.makepullrequestcomment(pull.base.repo.owner.login, 
                                   pull.base.repo.name,
                                   pull.number.toString,
                                   comment)
      notify ! CheckPullRequestDone(pull, job)
  }
}