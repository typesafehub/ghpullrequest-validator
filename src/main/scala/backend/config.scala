package backend

/** 
 * Configuration for running a particular verification.
 */
case class Config(githubUser: Credentials,
                  jenkinsUrl: String,
                  jenkinsUser: Credentials,
                  project: GithubProject, 
                  jenkinsJobs: Set[JenkinsJob])

case class Credentials(user: String, pw: String)
case class GithubProject(user: String, project: String)
case class JenkinsJob(name: String)
object JenkinsJob {
  implicit object OrderedJenkinsJob extends Ordering[JenkinsJob] {
    def compare(x: JenkinsJob, y: JenkinsJob): Int = x.name.compare(y.name)
  }
}