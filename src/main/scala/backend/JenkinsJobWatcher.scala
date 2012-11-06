package backend

import akka.actor.{Actor,ActorRef, ActorSystem,Props}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._


/** An actor that watches a specific job and returns its status when the job is completed. */
class JenkinsJobWatcher(api: JenkinsAPI, jobname: String, buildnumber: String, watcher: ActorRef) extends Actor {
  def receive: Receive = {
    case CheckJobDone => 
      val status = jobStatus
      if(!status.building) {
        watcher ! BuildResult(status.isSuccess, status.url)
        context stop self
      } else context.system.scheduler.scheduleOnce(1 minutes, self, CheckJobDone)
  }
  private def jobStatus = api.buildStatus(jobname, buildnumber)
  /** A timeout timer that wakes us up to check build status. */
}
