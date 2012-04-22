import net.liftweb.json.{ DefaultFormats, Formats }
import net.liftweb.json.JsonParser._

package object rest {
  implicit val formats = DefaultFormats // Brings in default date formats etc.
  def parseJsonTo[T](response: String)(implicit mf: Manifest[T], format: Formats) = parse(response).extract[T]
}
