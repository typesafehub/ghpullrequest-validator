
import github.{API=>gh}
import github.PullMini

case class Config(ghuser: String,
                  ghrepo: String, 
                  jenkinsUrl: String,
                  jenkinsJobs: Seq[String])

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
      val pr = gh.pullrequest(config.ghuser,config.ghrepo,info.number)
      
      for {
        job <- config.jenkinsJobs 
        msg <- buildBranch(job, pr.head.repo.git_url, pr.head.ref)
      } println(msg)
      // TODO - Write message on pull request.      
      
    }
    gh.pullrequests(config.ghuser,config.ghrepo) foreach validatePullRequest
  }
  
}