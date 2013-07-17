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

  private[this] val lastCheckedStamp = new collection.mutable.HashMap[String, String]
  private def checkPullRequests(ghuser: String, ghproject: String): Unit = {
    val b2ms = branchToMilestone(ghuser, ghproject)

    def key(num: String) = s"$ghuser/$ghproject#$num"
    def uptodate(p: PullMini): Boolean = {
      val k = key(p.number)
      val lastChecked = lastCheckedStamp.getOrElse(k, "")
      if (p.updated_at == lastChecked) log.warning(s"$k should be up-to-date, still checking... (${p.updated_at} == $lastChecked)")
      false // always check, TODO: replace by `p.updated_at == lastChecked` to only check new PRs (if this is reliable)
    }

    ghapi.pullrequests(ghuser, ghproject) filterNot uptodate foreach { pullMini =>
      lastCheckedStamp(key(pullMini.number)) = pullMini.updated_at
      listener ! CheckPullRequest(ghuser, ghproject, pullMini, b2ms)
    }
  }
}