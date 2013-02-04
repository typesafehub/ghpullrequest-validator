package rest
package jenkins

import dispatch.{Http => LoggingHttp, _}
import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._
import backend.JenkinsJob

/** May be used directly from any thread. */
import org.apache.http.auth.AuthScope
object Http extends LoggingHttp with NoLogging with thread.Safety  {
  type CurrentCredentials = util.DynamicVariable[Option[(AuthScope, Credentials)]]
}

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
  
  def buildJob(job: JenkinsJob, params: Map[String,String] = Map.empty): String = {
    val action = 
      if (params.isEmpty) makeReq("%s/build" format ( job.name))
      else {
        val uri = makeReq("job/%s/buildWithParameters" format (job.name))
        uri.POST << params
      }
    Http(action >- identity[String])
  }
  
  def buildStatus(job: JenkinsJob, buildNumber: String): BuildStatus = {
    val loc = makeReq("job/%s/%s/api/json" format (job.name, buildNumber))
    Http(loc >- parseJsonTo[BuildStatus])
  }
  
  /** A traversable that lazily pulls build status information from jenkins. */
  def buildStatusForJob(job: JenkinsJob): Stream[BuildStatus] = {
    val info = jobInfo(job)
    val reportedBuilds = info.builds.sorted

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
              Build(number.toString, jenkinsUrl +"/job/"+ job.name +"/"+ number +"/")
            }
          } catch {
            case x: Exception =>
              List[Build]()
          }
        (reportedBuilds ++ additionalBuilds)
      }

    allBuilds.reverse.toStream.map(build => buildStatus(job, build.number))
  }
    // new Traversable[BuildStatus]{
    //   override def foreach[U](f: BuildStatus => U): Unit = {
    //     val info = jobInfo(job)
    //     // println("buildStatusForJob: "+ job.name +" -> "+ info)
    //     for {
    //       build <- info.builds.sorted.reverse
    //       status = buildStatus(job, build.number)
    //     } try f(status) catch {
    //       case t: Exception => // Ignore this one.
    //     }
    //   }
    // }
}

object API {
  def apply(url: String, auth: Option[(String,String)] = None) = new API(url, auth)
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

case class QueueItem(buildable: Boolean)