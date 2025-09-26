package oxygen.sql.schema

import oxygen.predef.core.*

final case class Column(
    name: String,
    columnType: Column.Type,
    nullable: Boolean,
) {

  def toSql: String =
    s"$name ${columnType.show} ${if (nullable) "NULL" else "NOT NULL"}"

  def prefixed(prefix: String): Column =
    if (name.isEmpty) copy(name = prefix)
    else copy(name = s"${prefix}_$name")

}
object Column {

  given Show[Column] = _.toSql

  enum Type(final val show: String) {

    // Numeric Types
    case SmallInt extends Type("SMALLINT")
    case Int extends Type("INT")
    case BigInt extends Type("BIGINT")
    case Real extends Type("REAL")
    case DoublePrecision extends Type("DOUBLE PRECISION")
    // TODO (KR) : decimal

    // Character Types
    case Text extends Type("TEXT")
    // TODO (KR) : VARCHAR/CHAR

    // Date/Time Types
    case Timestamp extends Type("TIMESTAMP")
    case ZonedTimestamp extends Type("TIMESTAMP WITH TIME ZONE")
    case Date extends Type("DATE")
    case Time extends Type("TIME")
    // TODO (KR) : ZonedTime
    // TODO (KR) : Interval

    // Boolean Types
    case Boolean extends Type("BOOLEAN")

    // UUID Types
    case UUID extends Type("UUID")

    // Json Types
    case Json extends Type("JSON")
    case Jsonb extends Type("JSONB")

    // Binary Types
    // TODO (KR) : ...

    // Array Types
    // TODO (KR) : ...

  }
  object Type {
    given Show[Column.Type] = _.show
  }

}
