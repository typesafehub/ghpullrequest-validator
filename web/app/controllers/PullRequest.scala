package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import backend.{Config,Credentials,GithubProject}

object PullRequest extends Controller {
  
  private val credentialsMapping = mapping(
    "user" -> text,
    "password" -> text
  )(Credentials.apply)(Credentials.unapply)

  private val projectMapping = mapping(
    "user" -> text,
    "project" -> text
  )(GithubProject.apply)(GithubProject.unapply)

  private val setupForm = Form(
    mapping(
      "githubUser" -> credentialsMapping,
      "jenkinsUrl" -> text,
       "jenkinsUser" -> credentialsMapping,
       "project" -> projectMapping,
       "jenkinsJobs" -> Forms.list(text)
    )(ConfigHelper.apply)(ConfigHelper.unapply)
  )

  // Hack to work around "list" supported in play, but not set?
  object ConfigHelper {
    def apply(githubUser: Credentials, jenkinsUrl: String, jenkinsUser: Credentials, project: GithubProject, jobs: List[String]) =
      Config(githubUser, jenkinsUrl, jenkinsUser, project, jobs.toSet)
    def unapply(x: Any) = x match {
      case Config(githubUser, jenkinsUrl, jenkinsUser, project, jobs) =>
        Some(githubUser, jenkinsUrl, jenkinsUser, project, jobs.toList)
      case _ => None
    }
  }

  def list = Action { implicit request =>
    Ok(views.html.index(Seq.empty, setupForm))
  }
  def submit = Action { implicit request =>
    
    Ok(views.html.index(Seq.empty, setupForm))
  }
  
}
