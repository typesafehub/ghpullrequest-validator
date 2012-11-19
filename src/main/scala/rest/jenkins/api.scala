package rest
package jenkins

import dispatch._
import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._
import backend.JenkinsJob

/**
 * An API class to connect to jenkins and do stuff.
 */
class API(jenkinsUrl: String, auth: Option[(String,String)] = None) {

  protected def makeReq(uri: String) = {
    val url = dispatch.url("%s/%s" format (jenkinsUrl, uri))
    auth match {
      case Some((user,pw)) => url.as_!(user, pw)
      case _               => url
    }
  }
  
  def jobInfo(job: JenkinsJob): Job = {
    val loc = makeReq("job/%s/api/json" format (job.name))
    Http(loc >- parseJsonTo[Job])
  }
  
  def buildJob(job: JenkinsJob, params: Map[String,String] = Map.empty): Unit = {
    val action = 
      if (params.isEmpty) makeReq("%s/build" format ( job.name))
      else {
        val uri = makeReq("job/%s/buildWithParameters" format (job.name))
        uri.POST << params
      }
    Http(action >|)
  }
  
  def buildStatus(job: JenkinsJob, buildNumber: String): BuildStatus = {
    val loc = makeReq("job/%s/%s/api/json" format (job.name, buildNumber))
    Http(loc >- parseJsonTo[BuildStatus])
  }
  
  /** A traversable that lazily pulls build status information from jenkins. */
  def buildStatusForJob(job: JenkinsJob): Traversable[BuildStatus] = 
    new Traversable[BuildStatus]{
      override def foreach[U](f: BuildStatus => U): Unit = {
        val info = jobInfo(job)
        for {
          build <- info.builds.sorted.reverse 
          status = buildStatus(job, build.number)
        } try f(status) catch {
          case t: Exception => // Ignore this one.
        }
      }
    }
}

object API {
  def apply(url: String, auth: Option[(String,String)] = None) = new API(url, auth)
}

case class Job(name: String,
               description: String,
               nextBuildNumber: String,
               builds: List[Build])
   
case class Build(number: String, url: String) extends Ordered[Build] {
  def num: Int = number.toInt
  def compare(that: Build) = this.num - that.num
}

case class Param(name: String, value: String) {
  override def toString = "%s -> %s" format (name, value)
}
case class Actions(parameters: List[Param]) {
  override def toString = "Parameters(%s)" format(parameters mkString ", ")
}
case class BuildStatus(number: String, 
    result: String, 
    building: Boolean, 
    id: String,
    timestamp: String,
    duration: String,
    actions: Actions,
    url: String
) {
  def isSuccess = result == "SUCCESS"
    
  // TODO - Is this ok to assume?  
  def timestampDate =
    new java.util.Date(timestamp.toLong)
}
