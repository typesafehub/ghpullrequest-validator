package rest
package github

import dispatch._

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


trait API {
  import Authenticate.USER_AGENT

  private def makeAPIurl(uri: String) = url("https://api.github.com" + uri) <:< Map(
      "Authorization" -> "token %s".format(token),
      "User-Agent" -> USER_AGENT)

  val token: String


  /** Pulls in all the pull requests. */
  def pullrequests(user: String, repo: String): List[PullMini] = {
    val url = makeAPIurl("/repos/%s/%s/pulls?per_page=100" format (user,repo))
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
  
  // PATCH /repos/:owner/:repo/issues/comments/:id
  def editPRComment(user: String, repo: String, id: String, body: String): Comment = {
    val url = makeAPIurl("/repos/%s/%s/issues/comments/%s" format (user, repo, id))
    val json = IssueComment(body).toJson
    val action = (url.copy(method="PATCH") << json >- parseJsonTo[Comment])
    Http(action)
  }

  
  def commitStatus(user: String, repo: String, commitsha: String): List[CommitStatus] = {
    val url    = makeAPIurl("/repos/%s/%s/statuses/%s" format (user, repo, commitsha))
    val action = url >- parseJsonTo[List[CommitStatus]]
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
  def createLabel(user: String, repo: String, label: Label): List[Label] = {
    val url = makeAPIurl("/repos/%s/%s/labels" format (user, repo))
    val action = (url.POST << makeJson(label) >- parseJsonTo[List[Label]])
    Http(action)
  }


}

object API {
  def apply(auth: Authorization): API = 
    new { override val token = auth.token } with API {}

  def fromUser(user: String, pw: String): API = 
    apply(Authenticate.authenticate(user, pw))
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
  mergeable: Boolean
) extends Ordered[Pull] {
  def compare(other: Pull): Int = number compare other.number
  def sha10  = head.sha10
  def ref    = head.ref
  def branch = head.label.replace(':', '/')
  def date   = updated_at takeWhile (_ != 'T')
  def time   = updated_at drop (date.length + 1)
}

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
  commit: CommitInfo
)

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
  // Github Added
  id: Option[String] = None,
  created_at: Option[String]=None,
  updated_at: Option[String]=None,
  url: Option[String]=None,
  creator: Option[User]=None) {
  def toJson = makeJson(this)

  import CommitStatus._

  def forJob(job: String) = description match { case Some(s) if s.startsWith(job) => true case _ => false }
  // jenkins job is running
  def pending = state == PENDING
  // jenkins job was successful
  def success = state == SUCCESS
  // jenkins job found an error
  def error   = state == ERROR

  // something went wrong
  def failed  = state == FAILURE
}
object CommitStatus {
  final val PENDING = "pending"
  final val SUCCESS = "success"
  final val ERROR = "error"
  final val FAILURE = "failure"

  def jobStarted(name: String, url: String) = CommitStatus(PENDING, Some(url), Some(name +" started."))
  def jobEnded(name: String, url: String, ok: Boolean, message: String) =
    CommitStatus(if(ok) SUCCESS else ERROR, Some(url), Some((name +": "+ message).take(140)))
}


case class IssueComment(body: String) {
  // import net.liftweb.json._
  // import JsonAST._
  // import Printer._

  def toJson = makeJson(this) //pretty(render(JObject(List(JField("body", JString(body))))))
}

case class Label(name: String, color: String = "FFFFFF", url: Option[String] = None) {
  override def equals(o: Any) = o match {
    case Label(`name`, `color`, _) => true
    case _ => false
  }
}
