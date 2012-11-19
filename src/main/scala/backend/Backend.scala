package backend

import akka.actor.{ActorRef,ActorRefFactory,Props}
import rest.github.{API=>GithubAPI}
import rest.jenkins.{API=>JenkinsAPI}

/** The interface for interacting with the Akka backend.  All calls are asynchronous. */
trait Backend {
  /** Notifies the back end to check a github project using the given jenkins jobs. */
  def checkPullRequestsOnSystem(ghuser: String, ghproject: String, jobs: Set[JenkinsJob]): Unit
}
object Backend {
  
  /** Constructs an actor system that takes "backend.CheckPullRequests" and
   * comments appropriately on the given project which jenkins jobs successfully passed.
   */
  def apply(ghapi: GithubAPI, japi: JenkinsAPI, system: ActorRefFactory): Backend = new Backend {
    private val backendSystem = {
      val jobBuilder = Props(new JenkinsJobBuilder(japi))
      val prchecker = Props(new PullRequestChecker(ghapi, jobBuilder))
      system.actorOf(Props(new GhPullPoller(ghapi, prchecker)))
    }
    def checkPullRequestsOnSystem(ghuser: String, ghproject: String, jobs: Set[JenkinsJob]): Unit =
      backendSystem ! CheckPullRequests(ghuser, ghproject, jobs)
  }

}