package backend

import akka.actor.{Actor,ActorSystem, Props}
import scala.concurrent.Future
import scala.concurrent.duration._

import akka.pattern.ask


/** Interface for interaction with a persisted github->jenkins configuration.
 * Send it config and it starts checking your pull requests.
 */
trait BackendStore {
  /** Adds a configuration to the backend that will result in checking
   * pull requests for a given project.
   */
  def addConfig(config: Config): Future[Seq[Config]]

  /** Returns all the configurations used in this backend. */
  def configs: Future[Seq[Config]]

  /** Removes a configuration from the list of configs. 
   * returns the new list of configurations.
   */
  def deleteConfig(config: Config): Future[Seq[Config]]
  // TODO - Save and restore?

  /** Start checking PRs in all projects every `periodMinutes` minutes */
  def startChecking(periodMinutes: Int): Unit
}
object BackendStore {
  def apply(system: ActorSystem, name: String): BackendStore = new BackendStore {
    lazy val backendStore = system.actorOf(Props[BackendStoreActor], name)
    
    def addConfig(config: Config): Future[Seq[Config]] = 
      ask(backendStore, config)(1 minutes).mapTo[Seq[Config]]
    
    def configs: Future[Seq[Config]] =
      ask(backendStore, GetCurrentConfigs)(1 minutes).mapTo[Seq[Config]]
    
    def deleteConfig(config: Config): Future[Seq[Config]] =
      ask(backendStore, DeleteConfig(config))(1 minutes).mapTo[Seq[Config]]

    def startChecking(periodMinutes: Int): Unit = backendStore ! CheckAllProjects(periodMinutes)
  }
}


case class CheckAllProjects(periodMins: Int)
case object GetCurrentConfigs
case class DeleteConfig(config: Config)

class BackendStoreActor extends Actor {
  import context.dispatcher // Use this Actors' Dispatcher as ExecutionContext

  final def receive: Receive = {
    case CheckAllProjects(period)   =>
      checkAllProjects()
      context.system.scheduler.scheduleOnce(period minutes, self, CheckAllProjects(period))
    case GetCurrentConfigs  => sendConfigsResponse()
    case c: Config          => 
      updateOrAddConfig(c)
      sendConfigsResponse()
    case DeleteConfig(config) =>
      // TODO - shutdown the backend rather than ignore it...?
      backends -= hashConfig(config)
      configs  -= hashConfig(config)
      sendConfigsResponse()
  }
  
  private def sendConfigsResponse(): Unit =
    sender ! (configs map (_._2) toSeq)
  
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
      val backend = Backend(ghapi, japi, c.jenkinsJobs, context, s"${c.project.user}+${c.project.project}")
      backends += (hashConfig(c) -> backend)
      backend
    })
    
  private final def updateOrAddConfig(c: Config): Unit =
    configs get hashConfig(c) match {
      case Some(c2) if c == c2 =>  
      case _                   => configs = configs.updated(hashConfig(c), c)
    }

  final def checkAllProjects(): Unit = {
    for {
      config <- configs.valuesIterator
      backend = backendFor(config)
    } backend.checkPullRequestsOnSystem(config.project.user, config.project.project)
  }
}

