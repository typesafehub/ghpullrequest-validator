import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._
import net.liftweb.json.Serialization

package object rest {
  implicit val formats = DefaultFormats // Brings in default date formats etc.
  def parseJsonTo[T](response: String)(implicit mf: Manifest[T], format: Formats) = parse(response).extract[T]
  def makeJson[T <: AnyRef](value: T)(implicit format: Formats) = Serialization.write(value)

  import dispatch.classic.{ Http => LoggingHttp, _ }

  /** May be used directly from any thread. */
  import org.apache.http.auth.AuthScope
  object Http extends LoggingHttp with NoLogging with thread.Safety {
    type CurrentCredentials = util.DynamicVariable[Option[(AuthScope, Credentials)]]
    // https://groups.google.com/forum/#!topic/dispatch-scala/RCtWZ5ZJuYo/discussion
    import org.apache.http.params.CoreConnectionPNames
    client.getParams.setParameter(CoreConnectionPNames.CONNECTION_TIMEOUT, 5000)
    client.getParams.setParameter(CoreConnectionPNames.SO_TIMEOUT, 10000)
  }
}
