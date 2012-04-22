package rest
package jenkins

import dispatch._
import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._

class API(jenkinsUrl: String) {
  
  def jobInfo(jobName: String): Job = {
    val loc = url("%s/job/%s/api/json" format (jenkinsUrl,jobName))
    Http(loc >- parseJsonTo[Job])
  }
  
  def buildJob(jobName: String, params: Map[String,String] = Map.empty): Unit = {
    val action = 
      if (params.isEmpty)
        url("%s/job/%s/build" format (jenkinsUrl, jobName))
      else {
        val uri = "%s/job/%s/buildWithParameters" format (jenkinsUrl, jobName)
        url(uri) <:< params
      }
    Http(action >|)
  }
  
  def buildStatus(jobName: String, buildNumber: String): BuildStatus = {
    val loc = url("%s/job/%s/%s/api/json" format (jenkinsUrl,jobName, buildNumber))
    Http(loc >- parseJsonTo[BuildStatus])
  }
}

object API {
  def apply(url: String) = new API(url)
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
