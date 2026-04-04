package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec
import oxygen.predef.core.*

final case class ColumnColumn(
    name: String,
    columnType: ColumnColumn.Type,
    nullable: Boolean,
) derives JsonCodec
object ColumnColumn {

  sealed abstract class Type(final val show: String) {

    override def toString: String = show

    val extension: Option[String] = None

  }
  object Type {

    sealed abstract class Single(show: String) extends Type(show) derives StrictEnum

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

    case object LTree extends Type.Single("ltree")

    // Binary Types
    // TODO (KR) : ...

    // Array Types
    final case class Array(elemType: Type) extends Type(elemType.show + "[]")
    final case class Vector(fixedSize: Option[Int]) extends Type(fixedSize.fold("vector")(s => s"vector($s)")) {
      override val extension: Option[String] = "vector".some
    }

    def decode(string: String): Either[String, ColumnColumn.Type] = string match
      case s"$inner[]"      => decode(inner).map(Array(_))
      case "vector"         => Vector(None).asRight
      case s"vector($size)" => size.toIntOption.toRight("invalid vector size").map(s => Vector(s.some))
      case _                => StrictEnum[Single].decodeEitherWithHint(string)

    given JsonCodec[Type] = JsonCodec.string.transformOrFail(decode, _.show)

  }

}
