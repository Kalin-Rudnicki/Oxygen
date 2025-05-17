package oxygen.sql.error

import oxygen.predef.core.*
import oxygen.sql.query.QueryContext
import oxygen.sql.schema.*

final case class QueryError(
    ctx: QueryContext,
    cause: QueryError.Cause,
) extends Throwable {

  def toIndentedString: IndentedString =
    IndentedString.section("Error executing sql query")(
      s"query-name: ${ctx.queryName}",
      IndentedString.section("cause:")(cause.toIndentedString),
      IndentedString.section("sql:")(ctx.sql),
    )

  override def getMessage: String =
    toIndentedString.toString("    ")

}
object QueryError {

  sealed trait Cause extends Throwable {

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
    val rowValues: Contiguous[Any]
  }
  object UnableToDecodeRow {
    final case class AtColumn(rowValues: Contiguous[Any], column: Column, idx: Int, message: String) extends UnableToDecodeRow
    final case class MapOrFail(rowValues: Contiguous[Any], index: Int, size: Int, valueToMap: Any, message: String) extends UnableToDecodeRow
    final case class InvalidRowSize(rowValues: Contiguous[Any], expected: Int, columns: Option[Columns[?]]) extends UnableToDecodeRow
  }

  final case class InvalidResultSetSize(expected: InvalidResultSetSize.ExpectedSize, actual: Int) extends QueryError.Cause
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
