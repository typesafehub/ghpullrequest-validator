package backend

/** 
 * Configuration for running a particular verification.
 */
case class Config(githubUser: Credentials,
                  jenkinsUrl: String,
                  jenkinsUser: Credentials,
                  project: GithubProject, 
                  jenkinsJobs: Set[String])

case class Credentials(user: String, pw: String)
case class GithubProject(user: String, project: String)