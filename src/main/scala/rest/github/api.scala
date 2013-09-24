package rest
package github

import dispatch.classic.{Http => _, _}

// note: it looks like the buildbot github user needs administrative permission to create labels,
// but also to set the commit status
object Authenticate {
  val USER_AGENT = "github.com/typesafehub/ghpullrequest-validator; thanks for breaking me again, github"
  private[this] val authorizations = :/("api.github.com").secure / "authorizations" <:< Map("User-Agent" -> USER_AGENT)

  val authScopes = """{
 "scopes": [ 
   "user", 
   "repo",
   "repo:status"
 ],
 "note": "rest.github.Authenticate API Access"
}"""

  /** This method looks for a previous GH authorization for this API and retrieves it, or
   * creates a new one.
   */
  def authenticate(user: String, pw: String): Authorization = {
    val previousAuth: Option[Authorization] =
      (getAuthentications(user,pw) filter (_.note == Some("rest.github.Authenticate API Access"))).headOption
    previousAuth getOrElse makeAuthentication(user, pw)
  }

 
  def makeAuthentication(user: String, pw: String): Authorization = 
    Http(authorizations.POST.as_!(user, pw) << authScopes >- parseJsonTo[Authorization])

  def getAuthentications(user: String, pw: String): List[Authorization] =
    Http(authorizations.as_!(user, pw) >- parseJsonTo[List[Authorization]])

  def deleteAuthentication(auth: Authorization, user: String, pw: String): Unit =
    Http( (authorizations / auth.id).DELETE.as_!(user,pw) >|)

  def deleteAuthentications(user: String, pw: String): Unit =
     getAuthentications(user, pw) foreach { a => 
       deleteAuthentication(a, user, pw) 
     }
}


class API(val token: String, val userName: String) {
  import Authenticate.USER_AGENT

  private def makeAPIurl(uri: String) = url("https://api.github.com" + uri) <:< Map(
      "Authorization" -> "token %s".format(token),
      "User-Agent" -> USER_AGENT)


  /** Pulls in all the pull requests. */
  def pullrequests(user: String, repo: String): List[PullMini] = {
    val url = makeAPIurl("/repos/%s/%s/pulls?per_page=100" format (user,repo))
    val action = url >- parseJsonTo[List[PullMini]]
    Http(action)
  } 

  def closedPullrequests(user: String, repo: String): List[PullMini] = {
    val url = makeAPIurl("/repos/%s/%s/pulls?per_page=100&state=closed" format (user,repo))
    val action = url >- parseJsonTo[List[PullMini]]
    Http(action)
  }

  /** Grabs the information for a single pull request. */
  def pullrequest(user: String, repo: String, number: String): Pull = {
    val url = makeAPIurl("/repos/%s/%s/pulls/%s?per_page=100" format (user,repo,number))
    val action = url >- parseJsonTo[Pull]
    Http(action)
  }
  
  def pullrequestcomments(user: String, repo: String, number: String): List[Comment] = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s/comments?per_page=100" format (user,repo,number))
    val action = url >- parseJsonTo[List[Comment]]
    Http(action)
  }

  def pullrequestcommits(user: String, repo: String, number: String): List[PRCommit] = {
    val url = makeAPIurl("/repos/%s/%s/pulls/%s/commits?per_page=100" format (user,repo,number))
    val action = url >- parseJsonTo[List[PRCommit]]
    Http(action)
  }

  def addPRComment(user: String, repo: String, number: String, comment: String): Comment = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s/comments" format (user, repo, number))
    val json = IssueComment(comment).toJson
    val action = (url.POST << json >- parseJsonTo[Comment])
    Http(action)
  }
  
  // DELETE /repos/:owner/:repo/pulls/comments/:number
  def deletePRComment(user: String, repo: String, id: String): Unit = {
    val url = makeAPIurl("/repos/%s/%s/pulls/comments/%s" format (user, repo, id))
    val action = (url.copy(method="DELETE") >|)
    Http(action)
  }

  // PATCH /repos/:owner/:repo/issues/comments/:id
  def editPRComment(user: String, repo: String, id: String, body: String): Comment = {
    val url = makeAPIurl("/repos/%s/%s/issues/comments/%s" format (user, repo, id))
    val json = IssueComment(body).toJson
    val action = (url.copy(method="PATCH") << json >- parseJsonTo[Comment])
    Http(action)
  }

  // most recent status comes first in the resulting list!
  def commitStatus(user: String, repo: String, commitsha: String): List[CommitStatus] = {
    val url    = makeAPIurl("/repos/%s/%s/statuses/%s" format (user, repo, commitsha))
    val action = url >- (json => parseJsonTo[List[CommitStatus]](json).sortBy(_.updated_at).reverse)
    Http(action)
  }

  def setCommitStatus(user: String, repo: String, commitsha: String, status: CommitStatus): CommitStatus = {
    val url    = makeAPIurl("/repos/%s/%s/statuses/%s" format (user, repo, commitsha))
    val json   = status.toJson
    val action = (url.POST << json >- parseJsonTo[CommitStatus])
    Http(action)
  }

  // POST /repos/:owner/:repo/issues/:number/labels
  def addLabel(user: String, repo: String, number: String, labels: List[String]): List[Label] = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s/labels" format (user, repo, number))
    val action = (url.POST << makeJson(labels) >- parseJsonTo[List[Label]])
    Http(action)
  }

  // DELETE /repos/:owner/:repo/issues/:number/labels/:name
  def deleteLabel(user: String, repo: String, number: String, label: String) = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s/labels/%s" format (user, repo, number, label))
    Http(url.DELETE >|)
  }

  // GET /repos/:owner/:repo/issues/:number/labels
  def labels(user: String, repo: String, number: String): List[Label] = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s/labels" format (user, repo, number))
    val action = (url >- parseJsonTo[List[Label]])
    Http(action)
  }

  // GET /repos/:owner/:repo/labels
  def allLabels(user: String, repo: String): List[Label] = {
    val url = makeAPIurl("/repos/%s/%s/labels" format (user, repo))
    val action = (url >- parseJsonTo[List[Label]])
    Http(action)
  }

  // POST /repos/:owner/:repo/labels
  def createLabel(user: String, repo: String, label: Label): Label = {
    val url = makeAPIurl("/repos/%s/%s/labels" format (user, repo))
    val action = (url.POST << makeJson(label) >- parseJsonTo[Label])
    Http(action)
  }

  // Create a commit comment
  // POST /repos/:owner/:repo/commits/:sha/comments
  def addCommitComment(user: String, repo: String, sha: String, comment: String): Comment = {
    val url = makeAPIurl("/repos/%s/%s/commits/%s/comments" format (user, repo, sha))
    val json = IssueComment(comment).toJson
    val action = (url.POST << json >- parseJsonTo[Comment])
    Http(action)
  }

  // List comments for a single commit
  // GET /repos/:owner/:repo/commits/:sha/comments
  def commitComments(user: String, repo: String, sha: String): List[Comment] = {
    val url = makeAPIurl("/repos/%s/%s/commits/%s/comments" format (user,repo,sha))
    val action = url >- parseJsonTo[List[Comment]]
    Http(action)
  }

  // Normalize sha if it's not 40 chars
  // GET /repos/:owner/:repo/commits/:sha
  def normalizeSha(user: String, repo: String, sha: String): String =
    if (sha.length == 40) sha
    else try {
      val url = makeAPIurl(s"/repos/$user/$repo/commits/$sha")
      val action = url >- (x => parseJsonTo[PRCommit](x).sha)
      Http(action)
    } catch {
      case e: Exception =>
        println(s"Error: couldn't normalize $sha (for $user/$repo): "+ e)
        sha
    }

  // DELETE /repos/:owner/:repo/comments/:id
  def deleteCommitComment(user: String, repo: String, id: String): Unit = {
    val url = makeAPIurl("/repos/%s/%s/comments/%s" format (user, repo, id))
    val action = (url.copy(method="DELETE") >|)
    Http(action)
  }


  //  GET /repos/:owner/:repo/milestones
  def repoMilestones(user: String, repo: String, state: String = "open"): List[Milestone] = {
    val url = makeAPIurl("/repos/%s/%s/milestones?state=%s" format (user, repo, state))
    val action = (url >- parseJsonTo[List[Milestone]])
    Http(action)
  }

  // PATCH /repos/:owner/:repo/issues/:number
  def setMilestone(user: String, repo: String, number: String, milestone: Int) = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s" format (user, repo, number))
    import net.liftweb.json._

    val action = (url.copy(method="PATCH") << makeJson(JObject(List(JField("milestone", JInt(milestone))))) >| )
    Http(action)
  }

  def issue(user: String, repo: String, number: String): Issue = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s" format (user,repo,number))
    val action = url >- parseJsonTo[Issue]
    Http(action)
  }

}

object API {
  def apply(auth: Authorization, user: String): API =
    new API(auth.token, user)

  def fromUser(user: String, pw: String): API = 
    apply(Authenticate.authenticate(user, pw), user)
}

case class AuthApp(name: String, url: String)
case class Authorization(id: String, token: String, app: AuthApp, note: Option[String])
case class PullMini(state: String,
                    number: String,
                    title: String,
                    body: String,
                    user: User,
                    updated_at: String) extends Ordered[PullMini] {
  def compare(other: PullMini): Int = number compare other.number
}

/** A link to something. You know, like a URL.*/
case class Link(href: String)
/** Lots of data structure have links.  This helps us use the right naming convention for them. */
trait HasLinks {
  def _links: Map[String, Link]
}
                       
case class User(
  login: String,
  name: Option[String],
  email: Option[String],
  repository: Option[Repository]
)

case class Repository(
  name: String,
  owner: User,
  url: String,
  git_url: String,
  updated_at: String,
  created_at: String,
  pushed_at: String
)

case class Pull(
  number: Int,
  head: GitRef,
  base: GitRef,
  user: User,
  title: String,
  body: String,
  state: String,
  updated_at: String,
  created_at: String,
  mergeable: Option[Boolean],
  milestone: Option[Milestone] // when treating an issue as a pull
) extends Ordered[Pull] {
  def compare(other: Pull): Int = number compare other.number
  def sha10  = head.sha10
  def ref    = head.ref
  def branch = head.label.replace(':', '/')
  def date   = updated_at takeWhile (_ != 'T')
  def time   = updated_at drop (date.length + 1)

  override def toString = s"${base.repo.owner.login}/${base.repo.name}#$number"
}

case class Issue(milestone: Option[Milestone])

case class GitRef(
  sha: String,
  label: String,
  ref: String,
  repo: Repository,
  user: User
) {
  def sha10 = sha take 10
}

case class PRCommit(
  sha: String,
  url: String,
  commit: CommitInfo) {
  // meh
  def shaMatches(other: String) = other.length >= 5 && sha.startsWith(other) || other.startsWith(sha)
}

case class CommitInfo(
  committer: CommitAuthor,
  author: CommitAuthor,
  message: String
)

case class CommitAuthor(
  email: String,
  name: String,
  date: String
)

case class Comment(
    url: String,
    id: String,
    body: String,
    user: User,
    created_at: String,
    updated_at: String)
    
case class CommitStatus(
  // User defined
  state: String,
  target_url: Option[String]=None,
  description: Option[String]=None,
  updated_at: Option[String]=None
  //  // Github Added
  //  id: Option[String] = None,
  //  created_at: Option[String]=None,
  //  url: Option[String]=None,
  //  creator: Option[User]=None
  ) {
  def toJson = makeJson(this)

  import CommitStatus._

  def job = description.flatMap(_.split(" ").headOption)

  def forJob(job: String) = description match { case Some(s) if s.startsWith(job) => true case _ => false }
  // jenkins job is running
  def pending = state == PENDING
  // jenkins job was successful
  def success = state == SUCCESS
  // jenkins job found an error
  def error   = state == ERROR

  // we don't add a SUCCESS job when there's other pending jobs waiting
  // we add a PENDING job with a description like "$job OK $message"
  def fakePending = {
    pending && description.flatMap(_.split(" ", 3).toList.drop(1).take(1).headOption).exists(_ == FAKE_PENDING)
  }
  def done    = success || error || fakePending

  def finishedUnsuccessfully = error || failed

  // something went wrong
  def failed  = state == FAILURE

  def stateString = (if (target_url.nonEmpty) "["+state+"]("+ target_url.get +")" else state)
  override def toString = stateString +": "+ description.getOrElse("")
}
object CommitStatus {
  final val PENDING = "pending"
  final val SUCCESS = "success"
  final val ERROR   = "error"
  final val FAILURE = "failure"

  // to distinguish PENDING jobs that are done but waiting on other PENDING jobs from truly pending jobs
  // the message of other PENDING jobs should never start with "$job OK"
  final val FAKE_PENDING = "OK"

  // TODO: assert(!name.contains(" ")) for all job* methods below
  // TODO: factor out the pattern
  def jobQueued(name: String) = CommitStatus(PENDING, None, Some(name +" queued."))
  def jobStarted(name: String, url: String) = CommitStatus(PENDING, Some(url), Some(name +" started."))
  // assert(!message.startsWith(FAKE_PENDING))
  def jobEnded(name: String, url: String, ok: Boolean, message: String) =
    CommitStatus(if(ok) SUCCESS else ERROR, Some(url), Some((name +" "+ message).take(140)))

  // only used for last commit
  def jobEndedBut(name: String, url: String, message: String)(prev: String) =
    CommitStatus(PENDING, Some(url), Some((name +" "+ FAKE_PENDING +" but waiting for "+ prev).take(140)))

  // depends on the invariant maintained by overruleSuccess so that we only have to look at the most recent status
  def jobDoneOk(cs: List[CommitStatus]) = cs.headOption.map(st => st.success || st.fakePending).getOrElse(false)

  /** Find the commit status that should precede the successful status for job `jobName` on commit `sha`,
   * to maintain the invariant that an error/pending status (without corresponding success) appears as the most recent status (per commit and per PR)
   */
  def overruleSuccess(ghapi: API, user: String, repo: String, sha: String, 
    jobName: String, statusUrl: String, message: String, priorCommits: List[PRCommit]): Option[CommitStatus] = {
    // this job completed successfully, find status that indicates another job may not be done yet with this commit
    val otherJobsNotDoneOk = notDoneOk(ghapi.commitStatus(user, repo, sha).filterNot(_.forJob(jobName)))
    otherJobsNotDoneOk.headOption orElse {
      // find earlier commit that has an unfinished/unsuccessful job
      priorCommits.toStream.flatMap(c =>
        notDoneOk(ghapi.commitStatus(user, repo, c.sha)).headOption.map(csNotOk => jobEndedBut(jobName, statusUrl, message)(" commit "+ c.sha.take(6) +" "+ csNotOk))
      ).headOption
    }
  }


  /** Find commit status that's either truly pending (not fake pending) or that found an error,
   * and for which there's no corresponding successful commit status
   */
  def notDoneOk(commitStati: List[CommitStatus]): Iterable[CommitStatus] = {
    val grouped  = commitStati.groupBy(_.job)
    val problems = grouped.flatMap {
      case (Some(jobName), jobAndCommitStati) if !jobAndCommitStati.exists(_.success) =>
        jobAndCommitStati.filter(cs => (cs.pending && !cs.fakePending) || cs.error)
      case _ =>
        Nil
    }
    // println("notDoneOk grouped: "+ grouped.mkString("\n"))
    // println("problems: "+ problems)
    problems
  }
}


case class IssueComment(body: String) {
  // import net.liftweb.json._
  // import JsonAST._
  // import Printer._

  def toJson = makeJson(this) //pretty(render(JObject(List(JField("body", JString(body))))))
}

case class Label(name: String, color: String = "FFFFFF", url: Option[String] = None) {
  override def equals(o: Any) = o match {
    case Label(`name`, _, _) => true
    case _ => false
  }
}

// {
//    "url": "https://api.github.com/repos/octocat/Hello-World/milestones/1",
//    "number": 1,
//    "state": "open",
//    "title": "v1.0",
//    "description": "",
//    "creator": {
//      "login": "octocat",
//      "id": 1,
//      "avatar_url": "https://github.com/images/error/octocat_happy.gif",
//      "gravatar_id": "somehexcode",
//      "url": "https://api.github.com/users/octocat"
//    },
//    "open_issues": 4,
//    "closed_issues": 8,
//    "created_at": "2011-04-10T20:09:31Z",
//    "due_on": null
//  }
//]
case class Milestone(number: Int, title: String, description: String) {
  // don't know how to ignore the trailing dot using java regexes, so using stripFinalDot...
  private val regex = "Merge to (\\S*)".r
  private def stripFinalDot(s: String) = (if(s.nonEmpty && s.last == '.') s.init else s).trim

  def mergeBranch =
    try regex.findFirstMatchIn(description).flatMap(m => m.subgroups.headOption.map(stripFinalDot))
    catch { case _: NullPointerException => None } // no idea how this happens, no time to find out
}