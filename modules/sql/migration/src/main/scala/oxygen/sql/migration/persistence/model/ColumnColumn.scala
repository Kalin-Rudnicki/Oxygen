package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import oxygen.predef.core.*

final case class ColumnColumn(
    name: String,
    columnType: ColumnColumn.Type,
    nullable: Boolean,
) derives JsonCodec
object ColumnColumn {

  enum Type(final val show: String) extends Enum[Type] {

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
  object Type extends Enum.Companion[Type] {

    override protected val defaultToString: Type => NonEmptyList[String] = t => NonEmptyList.one(t.show)

  }

}
