package oxygen.sql.error

import org.postgresql.util.{PSQLException, ServerErrorMessage}
import oxygen.predef.core.*

final case class PSQLError(e: PSQLException) {

  private val serverMessage: Option[ServerErrorMessage] = Option(e.getServerErrorMessage)

  val message: Option[String] = serverMessage.flatMap { m => Option(m.getMessage) }
  val detail: Option[String] = serverMessage.flatMap { m => Option(m.getDetail) }
  val hint: Option[String] = serverMessage.flatMap { m => Option(m.getHint) }
  val code: Option[PSQLCode] = Option(e.getSQLState).flatMap(PSQLCode.byCode.decodeOption)

  def toIndentedString: IndentedString =
    IndentedString.section("PSQL Error:")(
      message.map { c => IndentedString.keyValue("message: ", c) },
      detail.map { c => IndentedString.keyValue("detail: ", c) },
      hint.map { c => IndentedString.keyValue("hint: ", c) },
      code.map { c => IndentedString.keyValue("code: ", c.toString) },
      IndentedString.keyValue("raw-message: ", e.safeGetMessage),
    )

}
object PSQLError {

  def unapply(t: Throwable): Option[PSQLError] = t match
    case e: PSQLException => PSQLError(e).some
    case _                => None

}
