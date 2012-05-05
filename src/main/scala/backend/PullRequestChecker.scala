package backend

import akka.actor.{ActorRef,Actor, Props}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


case class CheckPullRequests(username: String, project: String, jobs: Set[String])
case class CheckPullRequest(pull: rest.github.Pull, jobs: Set[String])
case class CheckPullRequestDone(pull: rest.github.Pull, job: String)

/** A class that continually polls github for pull requests and notifies
 * a listener when they are discovered.
 */
class GhPullPoller(ghapi: GithubAPI, listenerProps: Props) extends Actor {
  
  // Create the listener of pull request checks as a nested actor so it's failures
  // get reported to us.
  // TODO - better way of disassociating these two...
  // Perhaps an actor that just grabs pull requests and sends messages for them...
  val listener = context.actorOf(listenerProps)
  
  def receive: Receive = {
    case CheckPullRequests(user, proj, jobs) => 
      checkPullRequests(user, proj, jobs)
  }
  
  private def checkPullRequests(ghuser: String, ghproject: String, jobs: Set[String]): Unit = 
    // TODO - cull pull requests that haven't changed since the last time we checked....
    for {
      p <- ghapi.pullrequests(ghuser, ghproject)
      pull <- catching(classOf[Exception]) opt 
                 ghapi.pullrequest(ghuser, ghproject, p.number.toString)
    } listener ! CheckPullRequest(pull, jobs)
}

/** This actor is responsible for validating that a pull request has had all required tests executed
 * and that they have passed.
 * 
 * Note: Any job sent to this actor must support one and only one build parameter,
 * "pullrequest"
 */
class PullRequestChecker(ghapi: GithubAPI, jobBuilderProps: Props) extends Actor {
  val jobBuilder = context.actorOf(jobBuilderProps)
  
  // cache of currently validating pull requests so we don't duplicate effort....
  var active = Set.empty[String]
  
  
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
      job <- jenkinsJobs
      if comment.body startsWith ("jenkins job " + job)
      // TODO - Check to see if *content* of pull request changed since this comment.
    } yield job

    def makeCommenter(job: String): ActorRef =
      context.actorOf(Props(new PullRequestCommenter(ghapi, pull, job, self)))
    
    // For all remaining verification jobs, spit out a new job.
    for {
      job <- (jenkinsJobs -- checkedBuilds).toSeq.sorted
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