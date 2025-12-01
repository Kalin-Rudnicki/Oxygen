package oxygen.sql.schema

import oxygen.predef.core.*
import scala.annotation.tailrec

final case class Column(
    name: String,
    columnType: Column.Type,
    nullable: Boolean,
) {

  def toSql: String =
    s"$name ${columnType.show} ${if nullable then "NULL" else "NOT NULL"}"

  def prefixed(prefix: String): Column =
    if name.isEmpty then copy(name = prefix)
    else copy(name = s"${prefix}_$name")

}
object Column {

  given Show[Column] = _.toSql

  sealed abstract class Type(final val show: String) {

    @tailrec
    final def baseType: String = this match
      case single: Type.Single  => single.show
      case Type.Array(elemType) => elemType.baseType

  }
  object Type {
    given Show[Column.Type] = _.show

    sealed abstract class Single(show: String) extends Type(show)

    // Numeric Types
    case object SmallInt extends Type.Single("SMALLINT")
    case object Int extends Type.Single("INT")
    case object BigInt extends Type.Single("BIGINT")
    case object Real extends Type.Single("REAL")
    case object DoublePrecision extends Type.Single("DOUBLE PRECISION")
    // TODO (KR) : decimal

    // Character Types
    case object Text extends Type.Single("TEXT")
    // TODO (KR) : VARCHAR/CHAR

    // Date/Time Types
    case object Timestamp extends Type.Single("TIMESTAMP")
    case object ZonedTimestamp extends Type.Single("TIMESTAMP WITH TIME ZONE")
    case object Date extends Type.Single("DATE")
    case object Time extends Type.Single("TIME")
    // TODO (KR) : ZonedTime
    // TODO (KR) : Interval

    // Boolean Types
    case object Boolean extends Type.Single("BOOLEAN")

    // UUID Types
    case object UUID extends Type.Single("UUID")

    // Json Types
    case object Json extends Type.Single("JSON")
    case object Jsonb extends Type.Single("JSONB")

    // Binary Types
    // TODO (KR) : ...

    // Array Types
    final case class Array(elemType: Column.Type) extends Type(elemType.show + "[]")

  }

}
