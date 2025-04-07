package oxygen.json

import oxygen.predef.core.*
import scala.collection.mutable

sealed trait Json {

  private[Json] def writeSimple(sb: mutable.StringBuilder): Unit

  private[Json] def writePretty(sb: mutable.StringBuilder, indent: String): Unit =
    writeSimple(sb)

  final def showSimple: String = {
    val sb = mutable.StringBuilder(128)
    writeSimple(sb)
    sb.toString()
  }

  final def showPretty: String = {
    val sb = mutable.StringBuilder(128)
    writePretty(sb, "\n")
    sb.toString()
  }

  final def tpe: Json.Type = this match
    case Json.Str(_)    => Json.Type.String
    case _: Json.Number => Json.Type.Number
    case Json.Bool(_)   => Json.Type.Boolean
    case Json.Arr(_)    => Json.Type.Array
    case Json.Obj(_)    => Json.Type.Object
    case Json.Null      => Json.Type.Null

  override final def toString: String =
    showSimple

}
object Json {

  enum Type { case String, Number, Boolean, Array, Object, Null }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Cases
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Str(value: String) extends Json {

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit = {
      sb.append('"')
      value.foreach {
        case '\n' => sb.append("\\n")
        case '\t' => sb.append("\\t")
        case '\\' => sb.append("\\\\")
        case '"'  => sb.append("\\\"")
        case c    => sb.append(c)
      }
      sb.append('"')
    }

  }

  sealed trait Number extends Json

  final case class NumberWithDecimal(value: BigDecimal) extends Json.Number {

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit =
      sb.append(value.toString)

  }
  object Number {

    def unapply(number: Json.Number): Some[BigDecimal] = number match
      case NumberWithDecimal(value)    => Some(value)
      case NumberWithoutDecimal(value) => Some(BigDecimal(value))

  }

  final case class NumberWithoutDecimal(value: BigInt) extends Json.Number {

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit =
      sb.append(value.toString)

  }

  final case class Bool(value: Boolean) extends Json {

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit =
      sb.append(value.toString)

  }

  final case class Arr(value: IArray[Json]) extends Json {

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit = {
      sb.append('[')
      var already: Boolean = false
      value.foreach { v =>
        if (already) sb.append(',')
        else already = true
        v.writeSimple(sb)
      }
      sb.append(']')
    }

    override private[Json] def writePretty(sb: mutable.StringBuilder, indent: String): Unit = {
      val newIndent = indent + "  "
      sb.append('[')
      var already: Boolean = false
      value.foreach { v =>
        if (already) sb.append(',')
        else already = true
        sb.append(newIndent)
        v.writePretty(sb, newIndent)
      }
      sb.append(indent)
      sb.append(']')
    }

    override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
      case that: Json.Arr => this.value.toSeq == that.value.toSeq
      case _              => false

  }

  final case class Obj(value: IArray[(String, Json)]) extends Json {

    lazy val valueMap: Map[String, Json] = value.toMap

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit = {
      sb.append('{')
      var already: Boolean = false
      value.foreach { case (k, v) =>
        if (already) sb.append(',')
        else already = true
        Json.Str(k).writeSimple(sb)
        sb.append(':')
        v.writeSimple(sb)
      }
      sb.append('}')
    }

    override private[Json] def writePretty(sb: mutable.StringBuilder, indent: String): Unit = {
      val newIndent = indent + "  "
      sb.append('{')
      var already: Boolean = false
      value.foreach { case (k, v) =>
        if (already) sb.append(',')
        else already = true
        sb.append(newIndent)
        Json.Str(k).writePretty(sb, newIndent)
        sb.append(": ")
        v.writeSimple(sb)
      }
      sb.append(indent)
      sb.append('}')
    }

    override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
      case that: Json.Obj => this.valueMap == that.valueMap
      case _              => false
  }

  case object Null extends Json {

    override private[Json] def writeSimple(sb: mutable.StringBuilder): Unit =
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

  def parse(string: String): Either[JsonError, Json] =
    JsonParser.parse(string)

}
