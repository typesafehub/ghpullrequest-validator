package backend

import akka.actor.{ActorRef,Actor, Props, ActorLogging}
import scala.concurrent.duration._
import rest.github.{API=>GithubAPI, PullMini}


/** A class that continually polls github for pull requests and notifies
 * a listener when they are discovered.
 */
class GhPullPoller(ghapi: GithubAPI, pullRequestCheckerProps: Props) extends Actor with ActorLogging {
  // Create the pull request checker as a nested actor so its failures
  // get reported to us.
  // TODO - better way of disassociating these two...
  // Perhaps an actor that just grabs pull requests and sends messages for them...
  val listener = context actorOf pullRequestCheckerProps
  
  def receive: Receive = {
    case CheckPullRequests(user, proj) =>
      initLabels(user, proj)
      try checkPullRequests(user, proj)
      catch { case x: dispatch.classic.StatusCode =>
        log.error(s"Checking PRs on $user/$proj failed -- does the build bot lack admin permission?\n"+ x)
      }
    case PullRequestChecked(user, proj, pullMini) =>
      lastChecked(user, proj) = pullMini
  }
  
  private def initLabels(ghuser: String, ghproject: String) = {
    import rest.github.Label

    val requiredLabels  = Set(Label("reviewed", "02e10c"), Label("tested", "d7e102"), Label("needs-attention", "e10c02"))

    try {
      val availableLabels = ghapi.allLabels(ghuser, ghproject).toSet
      (requiredLabels -- availableLabels) foreach { l =>
        val created = ghapi.createLabel(ghuser, ghproject, l)
        log.debug("initLabels -- created $l as $created")
      }
    } catch {
      case x: net.liftweb.json.MappingException =>
        log.error(s"creating labels for $ghuser/$ghproject failed: "+ x)
    }
  }

  private def branchToMilestone(user: String, repo: String): Map[String, rest.github.Milestone] = {
    val miles = ghapi.repoMilestones(user, repo) // gets all open milestones
    log.debug("milestones: "+ miles)
    val branchToMS = miles.flatMap(m => m.mergeBranch.map((_, m))).toMap
    log.debug("branchToMS: "+ branchToMS)
    branchToMS
  }

  object lastChecked {
    def apply(user: String, proj: String, number: String) = map.getOrElse(key(user, proj, number), "")
    def update(user: String, proj: String, pullMini: PullMini) = map(key(user, proj, pullMini.number)) = pullMini.updated_at

    private val map = new collection.mutable.HashMap[String, String]
    private def key(user: String, proj: String, num: String) = s"$user/$proj#$num"
  }

  private def checkPullRequests(ghuser: String, ghproject: String): Unit = {
    val b2ms = branchToMilestone(ghuser, ghproject)

    def uptodate(p: PullMini): Boolean = {
      val lastCheckedAt = lastChecked(ghuser, ghproject, p.number)
      if (p.updated_at != lastCheckedAt) log.info(s"Detected $ghuser/$ghproject#${p.number} is out of date.")
      false // for now, always check, TODO: replace by `p.updated_at == lastChecked` to only check new PRs (if this turns out to be reliable)
    }

    ghapi.pullrequests(ghuser, ghproject) filterNot uptodate foreach { pullMini =>
      listener ! CheckPullRequest(ghuser, ghproject, pullMini, b2ms, self)
    }
  }
}