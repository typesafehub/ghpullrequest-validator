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
  val SPURIOUS_REBUILD = "SPURIOUS ABORT? -- PLS REBUILD "

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
    case BuildResult(status) =>
      val baseComment = "jenkins job %s: %s - %s" format (job.name, status.result, status.url)
      // make failures easier to spot
      val sadKitty = "\n![sad kitty](http://cdn.memegenerator.net/instances/100x/31464013.jpg)"

      def cleanPartestLine(line: String) = ("  - " + {
        try { line.split("/files/")(1).split("\\[FAILED\\]").head.trim }
        catch {
          case _: Exception => line
        }})

      val comment =
        status.result match {
          case "FAILURE" =>
            import dispatch._

            val consoleOutput = Http(url(status.url) / "consoleText" >- identity[String])
            val (log, failureLog) = consoleOutput.lines.span(! _.startsWith("BUILD FAILED"))

            (baseComment + sadKitty +"\n"+
              (if (log.exists(_.contains("test.suite"))) {
                log.filter(_.contains("[FAILED]")).map(cleanPartestLine).toList
              } else {
                failureLog.takeWhile(! _.startsWith("Total time: "))
              }).mkString("\n"))

          case "ABORTED" =>
            // if aborted and not rebuilt before, try rebuilding
            val comments = ghapi.pullrequestcomments(pull.base.repo.owner.login, pull.base.repo.name, pull.number.toString)
            baseComment + sadKitty +"\n"+ (
              if (comments.exists(_.body.contains(SPURIOUS_REBUILD))) "tried automatically rebuilding once before, not falling for it again!"
              else SPURIOUS_REBUILD + job.name
            )

          case _ => baseComment
        }

      ghapi.makepullrequestcomment(pull.base.repo.owner.login, 
                                   pull.base.repo.name,
                                   pull.number.toString,
                                   comment)
      notify ! CheckPullRequestDone(pull, job)
      // TODO - Can we kill ourselves now? I think so.
      context stop self
  }
}