import backend._
import collection.JavaConverters._
import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import com.typesafe.config.{
  Config=>TConfig,
  ConfigList,
  ConfigObject, 
  ConfigValue, 
  ConfigValueType, 
  ConfigFactory}

object GhApp extends scala.App {
  val actors =  ActorSystem()
  val system = BackendStore(actors)
  val log = Logging.getLogger(actors, this)
  
  if(configs.isEmpty) {
    log.error("No configuration defined!")
    System exit 1
  }
  
  // Load configuration to start system.
  for((name, c) <- configs) {
    log.debug("Adding config for: " + name)
    system addConfig c
  }
  
  // Pull either the specified configuration files, or nothing.  
  def configFiles: Seq[java.io.File] = 
    if(args.isEmpty) Seq(new java.io.File("ghpr.conf"))
    else             args map (a => new java.io.File(a))
  
  
  lazy val configs = for {
    file <- configFiles
    if file.exists
    config <- getConfigs(ConfigFactory parseFile file)
  } yield config
  
  def getConfigs(config: TConfig): Seq[(String,Config)] = {
    // TODO - useful error messages on failure.
    
    
    def configString(c: ConfigValue): Option[String] =
      if(c.valueType == ConfigValueType.STRING) Some(c.unwrapped.toString)
      else None
      
    def configObj(c: ConfigValue): Option[ConfigObject] =
      if(c.valueType == ConfigValueType.OBJECT) Some(c.asInstanceOf[ConfigObject])
      else None
      
    def configList(c: ConfigValue): Option[ConfigList] = 
      if(c.valueType == ConfigValueType.LIST)
        Some(c.asInstanceOf[ConfigList])
      else None

    def configStringList(c: ConfigValue): Option[Seq[String]] =
      configList(c) map { x =>
        x.iterator.asScala flatMap configString toSeq
      }
        
      
    def c2cred(c: ConfigObject): Option[Credentials] = 
      for {
        user <- configString(c.get("user"))
        pw <- configString(c.get("password"))
      } yield Credentials(user, pw)
    def c2proj(c: ConfigObject): Option[GithubProject] = 
      for {
        user <- configString(c.get("user"))
        project <- configString(c.get("project"))
      } yield GithubProject(user, project)
      
    
    def c2c(c: ConfigObject): Option[Config] =
      for {
        // Jenkins Config
        jo <- configObj(c.get("jenkins"))
        jenkinsUrl <- configString(jo.get("url"))
        jusero <- configObj(jo.get("user"))
        jenkinsUser <- c2cred(jusero)
        rawJobs <- configStringList(jo.get("jobs"))
        jobs = (rawJobs map JenkinsJob.apply).toSet
        // Github config
        gho <-configObj(c.get("github"))
        ghuo <- configObj(gho.get("user"))
        ghuser <- c2cred(ghuo)
        gpo <- configObj(gho.get("project"))
        project  <- c2proj(gpo)
      } yield Config(ghuser, jenkinsUrl, jenkinsUser, project, jobs)

    val configs = 
      for {
        kv <- config.root.entrySet.iterator.asScala
        name = kv.getKey.toString
        obj <- configObj(kv.getValue)
        c <- c2c(obj)
      } yield name -> c
    configs.toSeq
  }
}