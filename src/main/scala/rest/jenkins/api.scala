package rest
package jenkins

import dispatch.classic.{ Http => _, _ }
import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._
import backend.JenkinsJob

/** An API class to connect to jenkins and do stuff.
 */
class API(jenkinsUrl: String, auth: Option[(String, String)] = None) {

  protected def makeReq(uri: String) = {
    val req = url("%s/%s" format (jenkinsUrl, uri))
    auth match {
      case Some((user, pw)) => req.as_!(user, pw)
      case _                => req
    }
  }

  def buildJob(job: JenkinsJob, params: Map[String, String] = Map.empty): String = {
    val action =
      if (params.isEmpty) makeReq("%s/build" format (job.name))
      else {
        val uri = makeReq("job/%s/buildWithParameters" format (job.name))
        uri.POST << params
      }
    Http(action >- identity[String])
  }

  def buildStatus(job: JenkinsJob, buildNumber: String): Option[BuildStatus] =
    try {
      val loc = makeReq("job/%s/%s/api/json" format (job.name, buildNumber))
      Some(Http(loc >- parseJsonTo[BuildStatus]))
    } catch {
      case e@(_: dispatch.classic.StatusCode | _: net.liftweb.json.MappingException) =>
        println(s"Error: could not get status for $job/$buildNumber: "+ e)
        None
    }

  /** A traversable that lazily pulls build status information from jenkins.
   *
   * Only statuses for the specified job (`job.name`) that have parameters that match all of `expectedArgs`
   */
  def buildStatusForJob(job: JenkinsJob, expectedArgs: Map[String,String]): Stream[BuildStatus] = try {
    val info = Http(makeReq("job/%s/api/json" format (job.name)) >- parseJsonTo[Job])
    val reportedBuilds = info.builds.sorted

    // hack: retrieve queued jobs from queue/api/json
    val queuedStati =
      try {
        Http(makeReq("queue/api/json") >- parseJsonTo[Queue]).items.filter(_.jobName == job.name).map(_.toStatus)
      } catch {
        case e@(_: dispatch.classic.StatusCode | _: net.liftweb.json.MappingException) =>
          println(s"Error: could not get queued jobs for $job: "+ e)
          Nil
      }


    // work around https://issues.jenkins-ci.org/browse/JENKINS-15583 -- jenkins not reporting all running builds
    // so hack it by closing the range from the last reported build to the lastBuild in the Json response, which is correct
    // only the builds part of the reply is wrong
    val start =
      try {
        if (reportedBuilds.isEmpty) info.firstBuild.number.toInt
        else reportedBuilds.last.number.toInt
      } catch { case x: Exception => 1 }

    val allBuilds = {
      val additionalBuilds = try {
        (start to info.lastBuild.number.toInt) map { number =>
          Build(number.toString, jenkinsUrl + "/job/" + job.name + "/" + number + "/")
        }
      } catch {
        case x: Exception =>
          List[Build]()
      }
      (reportedBuilds ++ additionalBuilds)
    }

    // queued items must come first, they have been added more recently or they wouldn't have been queued
    val all = queuedStati.toStream ++ allBuilds.reverse.toStream.flatMap(b => buildStatus(job, b.number))
    all.filter { status =>
      val paramsForExpectedArgs = status.actions.parameters.collect {
        case Param(n, Some(v)) if expectedArgs.isDefinedAt(n) => (n, v)
      }.toMap

      paramsForExpectedArgs == expectedArgs
    }
  } catch {
    case e@(_: dispatch.classic.StatusCode | _: net.liftweb.json.MappingException) =>
      println(s"Error: could not get buildStatusForJob for $job: "+ e)
      Stream.empty[BuildStatus]
  }

}

object API {
  def apply(url: String, auth: Option[(String, String)] = None) = new API(url, auth)
}

case class Job(name: String,
  description: String,
  nextBuildNumber: String,
  builds: List[Build],
  queueItem: Option[QueueItem],
  lastBuild: Build,
  firstBuild: Build)

case class Build(number: String, url: String) extends Ordered[Build] {
  def num: Int = number.toInt
  def compare(that: Build) = this.num - that.num
}

// value is optional: password-valued parameters hide their value
case class Param(name: String, value: Option[String]) {
  override def toString = "%s -> %s" format (name, value)
}
case class Actions(parameters: List[Param]) {
  override def toString = "Parameters(%s)" format (parameters mkString ", ")
}
case class BuildStatus(number: String,
  result: String,
  building: Boolean,
  id: String,
  timestamp: String,
  duration: String,
  actions: Actions,
  url: String) {

  assert(!(building && queued), "Cannot both be building and queued.")

  def queued = false
  def isSuccess = result == "SUCCESS"

  // TODO - Is this ok to assume?  
  def timestampDate =
    new java.util.Date(timestamp.toLong)
}

case class Queue(items: List[QueueItem])
case class QueueItem(actions: Actions, task: Task, id: String) {
  def jobName = task.name
  // the url is fake but needs to be unique
  def toStatus = new BuildStatus("0", "Queued build for " + task.name + " id: " + id, false, "0", "0", "0", actions, task.url + "/queued/" + id) { override def queued = true }
}
case class Task(name: String, url: String)
