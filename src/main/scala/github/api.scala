package github

import dispatch._

import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._


object API {
  private implicit val formats = DefaultFormats // Brings in default date formats etc.
  private def makeAPIurl(uri: String) = url("https://api.github.com" + uri)
  private def parseJsonTo[T](response: String)(implicit formats: Formats, mf: Manifest[T]) = parse(response).extract[T]
  
  /** Pulls in all the pull requests. */
  def pullrequests(user: String, repo: String): List[PullMini] = {
    val url = makeAPIurl("/repos/%s/%s/pulls" format (user,repo))
    val action = url >- parseJsonTo[List[PullMini]]
    Http(action)
  } 
  /** Grabs the information for a single pull request. */
  def pullrequest(user: String, repo: String, number: String): Pull = {
    val url = makeAPIurl("/repos/%s/%s/pulls/%s" format (user,repo,number))
    val action = url >- parseJsonTo[Pull]
    Http(action)
  }
  
  def pullrequestcomments(user: String, repo: String, number: String): List[Comment] = {
    val url = makeAPIurl("/repos/%s/%s/pulls/%s/comments" format (user,repo,number))
    val action = url >- parseJsonTo[List[Comment]]
    Http(action)
  }
    
}




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
  git_url: String
)

case class Pull(
  number: Int,
  head: Commit,
  user: User,
  title: String,
  body: String,
  state: String,
  updated_at: String,
  mergeable: Boolean,
  base: Commit
) extends Ordered[Pull] {
  def compare(other: Pull): Int = number compare other.number
  def sha10  = head.sha10
  def ref    = head.ref
  def branch = head.label.replace(':', '/')
  def date   = updated_at takeWhile (_ != 'T')
  def time   = updated_at drop (date.length + 1)
}

case class Commit(
  sha: String,
  label: String,
  ref: String,
  repo: Repository,
  user: User
) {
  def sha10 = sha take 10
}

case class Comment(
    url: String,
    id: String,
    body: String,
    user: User,
    created_at: String,
    updated_at: String)
