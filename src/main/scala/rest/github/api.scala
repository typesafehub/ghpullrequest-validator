package rest
package github

import dispatch._

object Authenticate {
  private[this] val authorizations = :/("api.github.com").secure / "authorizations"

  val authScopes = """{
 "scopes": [ 
   "user", 
   "repo" 
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
  private def makeAPIurl(uri: String) = url("https://api.github.com" + uri) <:< Map("Authorization" -> "token %s".format(token))

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

  def pullrequestcommits(user: String, repo: String, number: String): List[CommitInfo] = {
    val url = makeAPIurl("/repos/%s/%s/pulls/%s/commits?per_page=100" format (user,repo,number))
    val action = url >- parseJsonTo[List[CommitInfo]]
    Http(action)
  }

  def makepullrequestcomment(user: String, repo: String, number: String, comment: String): Comment = {
    val url = makeAPIurl("/repos/%s/%s/issues/%s/comments" format (user, repo, number))
    val json = IssueComment(comment).toJson
    val action = (url.POST << json >- parseJsonTo[Comment])
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

case class CommitInfo(
  sha: String,
  url: String,
  author: User,
  committer: User, 
  commit: Commit,
  parents: List[CommitRef]
)

case class Commit(
  url: String,
  message: String,
  tree: CommitRef,
  author: CommitAuthor,
  committer: CommitAuthor  
)

case class CommitRef(
  sha: String,
  url: String)

case class CommitAuthor(
  name: String,
  date: String,
  email: String
)

case class Comment(
    url: String,
    id: String,
    body: String,
    user: User,
    created_at: String,
    updated_at: String)


case class IssueComment(body: String) {
  def toJson = """{ "body": "%s" }""".stripMargin format (body.replaceAll("\"", "\\\""))
}
