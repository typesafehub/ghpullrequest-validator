package backend

import akka.actor.{ActorRef, Actor, Props, ActorLogging}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


/** Comments on a given pull request with the result of a jenkins job. 
 * Note: This only helps the PulLRequestValidator actors and should not be used
 * stand-alone.
 */
class PullRequestCommenter(ghapi: GithubAPI, pull: rest.github.Pull, job: JenkinsJob, notify: ActorRef) extends Actor with ActorLogging {
  def receive: Receive = {
    case BuildStarted(url) =>
      // add job started comment if we hadn't already
      val message  = "Started jenkins job %s at %s" format (job.name, url)
      val comments = ghapi.pullrequestcomments(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString)
      if (comments.exists(_.body == message)) log.debug("DOUBLE RAINBOW^W start comment! "+ message)
      else {
        comments.filter(_.body.startsWith("Started jenkins job "+ job.name)) match {
          case Nil =>
          case otherStarts => log.debug("other start comments exist! "+ otherStarts)
        }
        val comment  = ghapi.makepullrequestcomment(pull.base.repo.owner.login,
                                       pull.base.repo.name,
                                       pull.number.toString,
                                       message)
        log.debug("Commented job "+ job.name +" started at "+ url +" res: "+ comment)
      }
    case BuildResult(success, url) =>
      val successString = if(success) "Success" else "Failed"
      // make failures easier to spot
      val sadKitty      = if(success) "" else " <br> ![sad kitty](http://cdn.memegenerator.net/instances/100x/31464013.jpg)"
      val comment       = "jenkins job %s: %s - %s%s" format (job.name, successString, url, sadKitty)
      ghapi.makepullrequestcomment(pull.base.repo.owner.login, 
                                   pull.base.repo.name,
                                   pull.number.toString,
                                   comment)
      notify ! CheckPullRequestDone(pull, job)
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}