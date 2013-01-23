package backend

import akka.actor.{ActorRef,Actor, Props}
import akka.util.duration._
import rest.github.{API=>GithubAPI}
import util.control.Exception.catching


/** Comments on a given pull request with the result of a jenkins job. 
 * Note: This only helps the PulLRequestValidator actors and should not be used
 * stand-alone.
 */
class PullRequestCommenter(ghapi: GithubAPI, pull: rest.github.Pull, job: JenkinsJob, notify: ActorRef) extends Actor {
  def receive: Receive = {
    case BuildStarted(url) =>
      val comment = 
        "Started jenkins job %s at %s" format (job, url)
        ghapi.makepullrequestcomment(pull.base.repo.owner.login, 
                                     pull.base.repo.name,
                                     pull.number.toString,
                                     comment)
    case BuildResult(success, url) =>
      val successString = if(success) "Success" else "Failed"
      // make failures easier to spot
      val sadKitty      = if(success) "" else "![sad kitty](http://cdn.memegenerator.net/instances/100x/31464013.jpg)"
      val comment       = "jenkins job %s: %s - %s\n%s" format (job.name, successString, url, sadKitty)
      ghapi.makepullrequestcomment(pull.base.repo.owner.login, 
                                   pull.base.repo.name,
                                   pull.number.toString,
                                   comment)
      notify ! CheckPullRequestDone(pull, job)
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}