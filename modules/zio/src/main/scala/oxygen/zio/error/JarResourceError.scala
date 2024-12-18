package oxygen.zio.error

import oxygen.predef.core.*

sealed trait JarResourceError extends Throwable {

  val path: String

  override final def getMessage: String = this match {
    case JarResourceError.PathDNE(path) =>
      s"No such jar resource at path '$path'"
    case JarResourceError.Generic(path, cause) =>
      s"Encountered generic error when accessing jar resource at path '$path': ${cause.safeGetMessage}"
  }

}
object JarResourceError {
  final case class PathDNE(path: String) extends JarResourceError
  final case class Generic(path: String, cause: Throwable) extends JarResourceError
}
