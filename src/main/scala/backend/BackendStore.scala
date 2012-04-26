package backend

import akka.actor.{Actor,ActorSystem, Props}
import akka.util.duration._


/** Interface for interaction with a persisted github->jenkins configuration.
 * Send it config and it starts checking your pull requests.
 */
trait BackendStore {
  def addConfig(config: Config): Unit
  
  // TODO - Save and restore?
}
object BackendStore {
  def apply(system: ActorSystem): BackendStore = new BackendStore {
    lazy val backendStore = system.actorOf(Props(new BackendStoreActor))
    
    def addConfig(config: Config): Unit = backendStore ! config
  }
}


case object CheckAllProjects
class BackendStoreActor extends Actor {
  context.system.scheduler.scheduleOnce(1 second, self, CheckAllProjects)
  
  final def receive: Receive = {
    case c: Config        => updateOrAddConfig(c)
    case CheckAllProjects => checkAllProjects()
  }
  
  // Cache of generated backends.
  // TODO - this needs to be robust at some point.  For now, let's go for working and rebootable.
  var backends: Map[String, Backend] = Map.empty
  var configs: Map[String, Config] = Map.empty
  
  private final def hashConfig(c: Config): String  =
    "%s-%s-%s-%s-%s" format (c.jenkinsUrl, c.githubUser.user, c.jenkinsUser.user, c.project.user, c.project.project)
  
  
  private final def backendFor(c: Config): Backend = 
    backends getOrElse(hashConfig(c), {
      val japi = rest.jenkins.API(c.jenkinsUrl, Some(c.jenkinsUser.user -> c.jenkinsUser.pw))
      val ghapi = rest.github.API.fromUser(c.githubUser.user, c.githubUser.pw)
      val backend = Backend(ghapi, japi, context)
      backends += (hashConfig(c) -> backend)
      backend
    })
    
  private final def updateOrAddConfig(c: Config): Unit = {
    configs get hashConfig(c) match {
      case Some(c2) if c == c2 =>  
      case _                   => configs = configs.updated(hashConfig(c), c)
    }
    self ! CheckAllProjects
  }
  
  final def checkAllProjects(): Unit = {
    for {
      config <- configs.valuesIterator
      backend = backendFor(config)
    } backend.checkPullRequestsOnSystem(config.project.user, config.project.project, config.jenkinsJobs)
    context.system.scheduler.scheduleOnce(55 minutes, self, CheckAllProjects)
  }
}

