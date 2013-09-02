package backend

import akka.actor.{ActorRef,ActorRefFactory,Props}
import rest.github.{API=>GithubAPI}
import rest.jenkins.{API=>JenkinsAPI}

/** The interface for interacting with the Akka backend.  All calls are asynchronous. */
trait Backend {
  /** Notifies the back end to check a github project using the given jenkins jobs. */
  def checkPullRequestsOnSystem(ghuser: String, ghproject: String): Unit
}
object Backend {
  
  /** Constructs an actor system that takes "backend.CheckPullRequests" and
   * comments appropriately on the given project which jenkins jobs successfully passed.
   */
  def apply(ghapi: GithubAPI, japi: JenkinsAPI, jobs: Set[JenkinsJob], system: ActorRefFactory, name: String): Backend = new Backend {
    private val pullPoller = {
      val jobBuilder = JenkinsJobBuilder.props(japi)
      val prchecker  = PullRequestChecker.props(ghapi, jobs, jobBuilder)
      system.actorOf(GhPullPoller.props(ghapi, prchecker), name)
    }
    def checkPullRequestsOnSystem(ghuser: String, ghproject: String): Unit =
      pullPoller ! CheckPullRequests(ghuser, ghproject)
  }

}