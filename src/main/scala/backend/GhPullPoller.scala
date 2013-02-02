package backend

import akka.actor.{ActorRef,Actor, Props}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


/** A class that continually polls github for pull requests and notifies
 * a listener when they are discovered.
 */
class GhPullPoller(ghapi: GithubAPI, listenerProps: Props) extends Actor {
  
  // Create the listener of pull request checks as a nested actor so its failures
  // get reported to us.
  // TODO - better way of disassociating these two...
  // Perhaps an actor that just grabs pull requests and sends messages for them...
  val listener = context actorOf listenerProps
  
  def receive: Receive = {
    case CheckPullRequests(user, proj, jobs) => 
      checkPullRequests(user, proj, jobs)
  }
  
  private def checkPullRequests(ghuser: String, ghproject: String, jobs: Set[JenkinsJob]): Unit = 
    // TODO - cull pull requests that haven't changed since the last time we checked....
    for {
      p <- ghapi.pullrequests(ghuser, ghproject)
      pull <- catching(classOf[Exception]) opt 
                 ghapi.pullrequest(ghuser, ghproject, p.number.toString)
    } listener ! CheckPullRequest(pull, jobs)
}