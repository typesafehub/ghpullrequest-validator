
import rest.github.{API=>gh}
import rest.github.PullMini
import rest.jenkins.{API=>japi}

case class Config(ghuser: String,
                  ghpw: String,
                  ghrepo: String, 
                  jenkinsUrl: String,
                  jenkinsJobs: Seq[String]) {
  def jenkins = japi(jenkinsUrl)
  def github = gh.fromUser(ghuser, ghpw)
}

// TODO - Use futures for everything!                  

class ValidatePullRequests(config: Config) {
  
  /** Builds a git branch using the given jenkins job.  If there is a failure, returns some kind
   * of error message denoting how to find out the issue.
   */
  def buildBranch(jenkinsJob: String, gitUrl: String, refspec: String): Option[String] = {
    println("Building [%s] against [%s#%s]" format (jenkinsJob, gitUrl, refspec))
    None
  }
  
  def validatePullRequests(): Unit = {
    def validatePullRequest(info: PullMini): Unit = {
      val pr = config.github.pullrequest(config.ghuser,config.ghrepo,info.number)
      
      for {
        job <- config.jenkinsJobs 
        msg <- buildBranch(job, pr.head.repo.git_url, pr.head.ref)
      } println(msg)
      // TODO - Write message on pull request.      
      
    }
    config.github.pullrequests(config.ghuser,config.ghrepo) foreach validatePullRequest
  }
  
}
