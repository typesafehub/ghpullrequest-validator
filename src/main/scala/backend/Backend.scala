package backend

import akka.actor.{ActorRef,ActorSystem,Props}
import rest.github.{API=>GithubAPI}
import rest.jenkins.{API=>JenkinsAPI}

object Backend {
  
  /** Constructs an actor system that takes "backend.CheckPullRequests" and
   * comments appropriately on the given project which jenkins jobs successfully passed.
   */
  def makeSystem(ghapi: GithubAPI, japi: JenkinsAPI, system: ActorSystem): ActorRef = {
    val jobBuilder = system.actorOf(Props().withCreator(new JenkinsJobBuilder(japi)))
    val prchecker = system.actorOf(Props().withCreator(new PullRequestChecker(ghapi, jobBuilder)))
    val GhPullPoller = system.actorOf(Props().withCreator(new GhPullPoller(ghapi, prchecker)))
    GhPullPoller
  }
  
  /** Sends a message to the backend system to check the pull requests on a given
   * project using a given set of jobs.
   */
  def checkPullRequestsOnSystem(ghproject: (String,String), jobs: Set[String], checker: ActorRef): Unit = 
    checker ! CheckPullRequests(ghproject._1, ghproject._2, jobs)

}