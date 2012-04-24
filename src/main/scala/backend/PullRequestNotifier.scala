package backend

import akka.actor.{Actor,ActorRef}
import rest.github.{API=>GithubAPI}

case class FindUpdatesAfter(ghuser: String, ghproject: String, timestamp: String, listener: ActorRef)
case class PullRequest(pull: rest.github.Pull)

/** This class finds pull requests after a certain time.
 * TODO - actually use this...
 */
class PullRequestNotifier(val gh: GithubAPI) extends Actor {
  def receive: Receive = {
    case f @ FindUpdatesAfter(_,_,_,_) => findUpdatesAfter(f)
  }
  
  private def findUpdatesAfter(find: FindUpdatesAfter): Unit = {
    // Finds all pull requests after time X and returns them.
    val pulls = for {
      id <- gh.pullrequests(find.ghuser, find.ghproject)
      if id.updated_at > find.timestamp
      pull = gh.pullrequest(find.ghuser, find.ghproject, id.number)
    } find.listener ! PullRequest(pull)
  }
}