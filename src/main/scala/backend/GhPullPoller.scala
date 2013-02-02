package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


/** A class that continually polls github for pull requests and notifies
 * a listener when they are discovered.
 */
class GhPullPoller(ghapi: GithubAPI, listenerProps: Props) extends Actor with ActorLogging {
  
  // Create the listener of pull request checks as a nested actor so its failures
  // get reported to us.
  // TODO - better way of disassociating these two...
  // Perhaps an actor that just grabs pull requests and sends messages for them...
  val listener = context actorOf listenerProps
  
  def receive: Receive = {
    case CheckPullRequests(user, proj, jobs) =>
      initLabels(user, proj)
      checkPullRequests(user, proj, jobs)
  }
  
  private def initLabels(ghuser: String, ghproject: String) = {
    import rest.github.Label

    val requiredLabels  = Set(Label("reviewed", "02e10c"), Label("tested", "d7e102"), Label("needs-attention", "e10c02"))
    val availableLabels = ghapi.allLabels(ghuser, ghproject).toSet
    if (availableLabels != requiredLabels) {
      log.debug("initLabels -- available: "+ availableLabels)
      log.debug("initLabels -- required: "+ requiredLabels)

      (requiredLabels -- availableLabels) foreach { l =>
        log.debug("initLabels -- creating: "+ l +" --> "+ ghapi.createLabel(ghuser, ghproject, l))
      }
    }
  }

  private def checkPullRequests(ghuser: String, ghproject: String, jobs: Set[JenkinsJob]): Unit = 
    // TODO - cull pull requests that haven't changed since the last time we checked....
    for {
      p <- ghapi.pullrequests(ghuser, ghproject)
      pull <- catching(classOf[Exception]) opt 
                 ghapi.pullrequest(ghuser, ghproject, p.number.toString)
    } listener ! CheckPullRequest(pull, jobs)
}