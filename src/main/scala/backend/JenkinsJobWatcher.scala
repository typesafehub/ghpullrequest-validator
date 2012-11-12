package backend

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, ReceiveTimeout}
import rest.jenkins.{API => JenkinsAPI}
import akka.util.duration._


/** An actor that watches a specific job and returns its status when the job is completed. */
class JenkinsJobWatcher(api: JenkinsAPI, build: BuildProject, buildnumber: String, watcher: ActorRef) extends Actor with ActorLogging {
  log.debug("Watching for job finish: " + buildnumber)
  def receive: Receive = {
    case ReceiveTimeout => 
      val status = jobStatus
      if(!status.building) {
        watcher ! JobFinished(build, status)
        context stop self
      } else context setReceiveTimeout (1 minutes)
  }
  private def jobStatus = api.buildStatus(build.job, buildnumber)
  /** A timeout timer that wakes us up to check build status. */
  context setReceiveTimeout (2 minutes)
}
