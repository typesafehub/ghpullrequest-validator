package rest
package jenkins

import dispatch._
import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._

class API(jenkinsUrl: String, auth: Option[(String,String)] = None) {

  protected def makeReq(uri: String) = {
    val url = dispatch.url("%s/%s" format (jenkinsUrl, uri))
    auth match {
      case Some((user,pw)) => url.as_!(user, pw)
      case _               => url
    }
  }
  
  def jobInfo(jobName: String): Job = {
    val loc = makeReq("job/%s/api/json" format (jobName))
    Http(loc >- parseJsonTo[Job])
  }
  
  def buildJob(jobName: String, params: Map[String,String] = Map.empty): Unit = {
    val action = 
      if (params.isEmpty) makeReq("%s/build" format ( jobName))
      else {
        val uri = makeReq("job/%s/buildWithParameters" format (jobName))
        uri <:< params
      }
    Http(action >|)
  }
  
  def buildStatus(jobName: String, buildNumber: String): BuildStatus = {
    val loc = makeReq("job/%s/%s/api/json" format (jobName, buildNumber))
    Http(loc >- parseJsonTo[BuildStatus])
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

case class BuildStatus(number: String, 
    result: String, 
    building: Boolean, 
    id: String,
    timestamp: String,
    duration: String
    // TODO - Actions
) {
  def isSuccess = result == "SUCCESS"
}
