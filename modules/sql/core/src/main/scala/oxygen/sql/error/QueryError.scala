package oxygen.sql.error

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.sql.query.QueryContext
import oxygen.sql.schema.*

final case class QueryError(
    ctx: QueryContext,
    cause: QueryError.Cause,
) extends Throwable {

  private type Pos = (pos: Int, line: Int, inLine: Int)

  private def positionInfo: Option[Pos] =
    cause.optPosition.map { positionInfo(_) }

  private def positionInfo(pos: Int): Pos = {
    val str = ctx.sql.substring(0, pos - 1)
    val (line, inLine) = str.foldLeft((1, 1)) {
      case ((line, _), '\n')   => (line + 1, 1)
      case ((line, inLine), _) => (line, inLine + 1)
    }
    (pos, line, inLine)
  }

  private def showSql(pos: Pos): String = {
    val before = ctx.sql.substring(0, pos.pos - 1)
    val at = ctx.sql.charAt(pos.pos - 1)
    val after = ctx.sql.substring(pos.pos)

    val atColor =
      if (at.isWhitespace) at.unesc("").magentaBg
      else at.toString.magentaFg

    s"$before$atColor$after"
  }

  def toIndentedString: IndentedString = {
    val optPosInfo = positionInfo

    IndentedString.section("Error executing sql query")(
      s"query-name: ${ctx.queryContextHeader}",
      IndentedString.section("cause:")(cause.toIndentedString),
      optPosInfo.map { case (pos, line, inLine) =>
        IndentedString.keyValueSection("position:")(
          "pos: " -> pos.toString,
          "line: " -> line.toString,
          "inLine: " -> inLine.toString,
        )
      },
      IndentedString.section("sql:")(optPosInfo.fold(ctx.sql)(showSql)),
    )
  }

  override def getMessage: String =
    toIndentedString.toString("    ")

}
object QueryError {

  sealed trait Cause extends Throwable {

    final def optPosition: Option[Int] = this match
      case PSQL(e) => Option(e.e.getServerErrorMessage.getPosition).filterNot(_ == 0)
      case _       => None

    final def toIndentedString: IndentedString =
      this match {
        case error: DSLError =>
          IndentedString.section("Defect caused by incorrect use of query dsl")(
            s"hint: ${error.hint}",
            IndentedString.section("schema:")(error.schema.toIndentedString),
          )
        case Connection(connectionError) =>
          IndentedString.section("Connection error:")(connectionError.toIndentedString)
        case UnableToDecodeRow.AtColumn(rowValues, column, idx, message) =>
          IndentedString.section("Unable to decode column:")(
            IndentedString.keyValue("message: ", message),
            s"column: ${column.toSql}",
            s"index: $idx",
            IndentedString.keyValue("value: ", String.valueOf(rowValues(idx))),
            IndentedString.section("values:")(
              rowValues.zipWithIndex.map { case (v, i) => IndentedString.keyValue(s"[$i]: ", String.valueOf(v)) },
            ),
          )
        case UnableToDecodeRow.MapOrFail(rowValues, index, size, valueToMap, message) =>
          IndentedString.section("Error mapping query result:")(
            IndentedString.keyValue("message: ", message),
            IndentedString.keyValue("value-to-map: ", String.valueOf(valueToMap)),
            s"index: $index",
            s"size: $size",
            IndentedString.section("values:")(
              rowValues.zipWithIndex.map { case (v, i) => IndentedString.keyValue(s"[$i]: ", String.valueOf(v)) },
            ),
          )
        case UnableToDecodeRow.InvalidRowSize(rowValues, expected, columns) =>
          IndentedString.section("Invalid row size:")(
            s"expected: $expected",
            s"actual: ${rowValues.length}",
            IndentedString.section("values:")(
              rowValues.zipWithIndex.map { case (v, i) => IndentedString.keyValue(s"[$i]: ", String.valueOf(v)) },
            ),
            columns.map { columns =>
              IndentedString.section("columns:")(columns.columns.map(_.toSql))
            },
          )
        case InvalidResultSetSize(expected, actual) =>
          IndentedString.section("Invalid result set size:")(
            s"expected: $expected",
            s"actual: $actual",
          )
        case JDBCError(action, cause) =>
          IndentedString.section(s"JDBC error while trying to: $action")(
            IndentedString.section("cause:")(
              s"type: ${cause.getClass.getName}",
              s"cause: ${cause.safeGetMessage}",
            ),
          )
        case PSQL(psql) =>
          psql.toIndentedString
        case Generic(cause) =>
          IndentedString.section("Generic:")(
            s"type: ${cause.getClass.getName}",
            s"cause: ${cause.safeGetMessage}",
            s"trace: ${cause.getStackTrace.map(t => s"\n  - $t").mkString}",
          )
      }

  }
  object Cause {

    // TODO (KR) : parse specific errors
    def fromThrowable(cause: Throwable): QueryError.Cause = cause match
      case cause: QueryError.Cause => cause
      case PSQLError(e)            => PSQL(e)
      case cause                   => QueryError.Generic(cause)

  }

  sealed trait DSLError extends QueryError.Cause {

    val schema: RowRepr[?]

    def hint: String = this match
      case DSLError.NotAProductSchema(_)  => "Not a product schema"
      case DSLError.NotAnOptionSchema(_)  => "Not an option schema"
      case DSLError.NoSuchChild(_, child) => s"Invalid product field '$child'"

  }
  object DSLError {
    final case class NotAProductSchema(schema: RowRepr[?]) extends DSLError
    final case class NotAnOptionSchema(schema: RowRepr[?]) extends DSLError
    final case class NoSuchChild(schema: RowRepr.ProductRepr[?], child: String) extends DSLError
  }

  final case class Connection(connectionError: ConnectionError) extends QueryError.Cause

  sealed trait UnableToDecodeRow extends QueryError.Cause {
    val rowValues: ArraySeq[Any]
  }
  object UnableToDecodeRow {
    final case class AtColumn(rowValues: ArraySeq[Any], column: Column, idx: Int, message: String) extends UnableToDecodeRow
    final case class MapOrFail(rowValues: ArraySeq[Any], index: Int, size: Int, valueToMap: Any, message: String) extends UnableToDecodeRow
    final case class InvalidRowSize(rowValues: ArraySeq[Any], expected: Int, columns: Option[Columns[?]]) extends UnableToDecodeRow
  }

  final case class InvalidResultSetSize(expected: InvalidResultSetSize.ExpectedSize, actual: String) extends QueryError.Cause
  object InvalidResultSetSize {
    enum ExpectedSize {
      case Single
      case Optional
      case Range(min: Int, max: Int)
    }
  }

  // TODO (KR) : add specific cases (pk constraint, ex)

  final case class JDBCError(action: String, cause: Throwable) extends QueryError.Cause

  final case class PSQL(e: PSQLError) extends QueryError.Cause

  final case class Generic(cause: Throwable) extends QueryError.Cause

}
