package oxygen.sql.error

import oxygen.predef.core.*

sealed trait ConnectionError extends Throwable {

  final def toIndentedString: IndentedString =
    this match {
      case ConnectionError.PSQL(psql) =>
        psql.toIndentedString
      case ConnectionError.Generic(cause) =>
        IndentedString.section("Generic:")(
          s"type: ${cause.getClass.getName}",
          s"cause: ${cause.safeGetMessage}",
        )
    }

  override final def getMessage: String =
    toIndentedString.toString

}
object ConnectionError {

  final case class PSQL(psql: PSQLError) extends ConnectionError

  final case class Generic(cause: Throwable) extends ConnectionError

  // TODO (KR) : handle more specific cases
  def apply(cause: Throwable): ConnectionError = cause match
    case PSQLError(e) => PSQL(e)
    case _            => ConnectionError.Generic(cause)

}
