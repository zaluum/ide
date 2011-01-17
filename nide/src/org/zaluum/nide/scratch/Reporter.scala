package graystone.zaluum.interpreter
import scala.collection.mutable.Buffer

class Reporter {
  val errors = Buffer[String]()
  def +=(e:String) {errors +=e}
  def hasErrors = !errors.isEmpty
  def check = if (hasErrors) throw AbortingException(this)
  def cancel = throw AbortingException(this)
}
case class AbortingException(r:Reporter) extends Exception