package oxygen.schema

import scala.collection.mutable

sealed trait Json {

  protected def writeSimple(sb: mutable.StringBuilder): Unit

  protected def writePretty(sb: mutable.StringBuilder, indent: String): Unit =
    writeSimple(sb) // FIX-PRE-MERGE (KR) :

  final def showSimple: String = {
    val sb = mutable.StringBuilder(128)
    writeSimple(sb)
    sb.toString()
  }

  final def showPretty: String = {
    val sb = mutable.StringBuilder(128)
    writePretty(sb, "  ")
    sb.toString()
  }

  override final def toString: String =
    showSimple

}
object Json {

  enum Type { case String, Number, Boolean, Array, Object, Null }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Cases
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Str(value: String) extends Json {

    override protected def writeSimple(sb: mutable.StringBuilder): Unit = {
      ??? // TODO (KR) :
    }

  }

  sealed trait Number extends Json

  final case class NumberWithDecimal(value: BigDecimal) extends Json.Number {

    override protected def writeSimple(sb: mutable.StringBuilder): Unit = {
      ??? // TODO (KR) :
    }

  }
  object Number {

    def unapply(number: Json.Number): Some[BigDecimal] = number match
      case NumberWithDecimal(value)    => Some(value)
      case NumberWithoutDecimal(value) => Some(BigDecimal(value))

  }

  final case class NumberWithoutDecimal(value: BigInt) extends Json.Number {

    override protected def writeSimple(sb: mutable.StringBuilder): Unit = {
      ??? // TODO (KR) :
    }

  }

  final case class Bool(value: Boolean) extends Json {

    override protected def writeSimple(sb: mutable.StringBuilder): Unit = {
      ??? // TODO (KR) :
    }

  }

  final case class Arr(value: IArray[Json]) extends Json {

    override protected def writeSimple(sb: mutable.StringBuilder): Unit = {
      ??? // TODO (KR) :
    }

  }

  final case class Obj(value: IArray[(String, Json)]) extends Json {

    lazy val valueMap: Map[String, Json] = value.toMap

    override protected def writeSimple(sb: mutable.StringBuilder): Unit = {
      ??? // TODO (KR) :
    }

  }

  case object Null extends Json {

    override protected def writeSimple(sb: mutable.StringBuilder): Unit =
      sb.append("null")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def string(value: String): Json.Str = Json.Str(value)

  def number(value: Long): Json.Number = Json.number(BigInt(value))
  def number(value: Double): Json.Number = Json.number(BigDecimal(value))
  def number(value: BigInt): Json.Number = Json.NumberWithoutDecimal(value)
  def number(value: BigDecimal): Json.Number = Json.NumberWithDecimal(value)

  def boolean(value: Boolean): Json.Bool = Json.Bool(value)

  def arr(value: Json*): Json.Arr = Json.Arr(IArray.unsafeFromArray(value.toArray))

  def obj(value: (String, Json)*): Json.Obj = Json.Obj(IArray.unsafeFromArray(value.toArray))

  def `null`: Json.Null.type = Json.Null

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Parser
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def parseInternal(string: String): Option[Json] =
    ??? // TODO (KR) :

  def parse(string: String): Either[JsonError, Json] =
    parseInternal(string).toRight(JsonError(Nil, JsonError.Cause.DecodingFailed("Invalid Json")))

}
