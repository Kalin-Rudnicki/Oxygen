package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import oxygen.meta.K0
import oxygen.predef.core.*

final case class ColumnColumn(
    name: String,
    columnType: ColumnColumn.Type,
    nullable: Boolean,
) derives JsonCodec
object ColumnColumn {

  sealed abstract class Type(final val show: String) {
    override def toString: String = show
  }
  object Type {

    sealed abstract class Single(show: String) extends Type(show), Enum[Single]
    object Single extends Enum.Companion[Single] {
      override def values: scala.Array[Single] = K0.SumGeneric.EnumGeneric.deriveEnum.strictEnum.values[Single].toArray
    }

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
    final case class Array(elemType: Type) extends Type(elemType.show + "[]")

    def decode(string: String): Either[String, ColumnColumn.Type] = string match
      case s"$inner[]" => decode(inner).map(Array(_))
      case _           => Single.ToString.decode(string).toRight(s"invalid column type: $string")

    given JsonCodec[Type] = JsonCodec.string.transformOrFail(decode, _.show)

  }

}
