package oxygen.json

import oxygen.predef.core.*
import scala.collection.mutable

sealed trait Json {

  private[Json] def writeCompact(sb: mutable.StringBuilder): Unit

  private[Json] def writePretty(sb: mutable.StringBuilder, @scala.annotation.unused indent: String): Unit =
    writeCompact(sb)

  infix final def merge(that: Json): Json =
    (this, that) match {
      case (self: Json.Obj, that: Json.Obj) =>
        Json.Obj(
          (self.value.map(_._1) ++ that.value.map(_._1)).distinct.map { k =>
            (self.valueMap.get(k), that.valueMap.get(k)) match {
              case (Some(a), Some(b)) => k -> a.merge(b)
              case (Some(a), None)    => k -> a
              case (None, Some(b))    => k -> b
              case (None, None)       => ??? // not possible
            }
          },
        )
      case (self: Json.Arr, that: Json.Arr) =>
        Json.Arr(self.value.zipUsing(that.value)(identity, identity, _.merge(_)))
      case _ =>
        that
    }

  final def showCompact: String = {
    val sb = mutable.StringBuilder(128)
    writeCompact(sb)
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

  final def toJsonString: Option[Json.Str] = this match
    case json: Json.Str => json.some
    case _              => None

  final def toJsonBoolean: Option[Json.Bool] = this match
    case json: Json.Bool => json.some
    case _               => None

  final def toJsonArray: Option[Json.Arr] = this match
    case json: Json.Arr => json.some
    case _              => None

  final def toJsonObject: Option[Json.Obj] = this match
    case json: Json.Obj => json.some
    case _              => None

  final def toJsonNumber: Option[Json.Number] = this match
    case json: Json.Number => json.some
    case _                 => None

  final def toJsonNull: Option[Json.Null.type] = this match
    case Json.Null => Json.Null.some
    case _         => None

  override final def toString: String =
    showCompact

}
object Json {

  enum Type { case String, Number, Boolean, Array, Object, Null }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Cases
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Str(value: String) extends Json {

    override private[Json] def writeCompact(sb: mutable.StringBuilder): Unit = {
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

  final case class Number(value: BigDecimal) extends Json {

    override private[Json] def writeCompact(sb: mutable.StringBuilder): Unit =
      sb.append(value.toString)

  }

  final case class Bool(value: Boolean) extends Json {

    override private[Json] def writeCompact(sb: mutable.StringBuilder): Unit =
      sb.append(value.toString)

  }

  final case class Arr(value: Contiguous[Json]) extends Json {

    override private[Json] def writeCompact(sb: mutable.StringBuilder): Unit = {
      sb.append('[')
      var already: Boolean = false
      value.foreach { v =>
        if (already) sb.append(',')
        else already = true
        v.writeCompact(sb)
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

  }

  final case class Obj(value: Contiguous[(String, Json)]) extends Json {

    lazy val valueMap: Map[String, Json] = value.toMap

    override private[Json] def writeCompact(sb: mutable.StringBuilder): Unit = {
      sb.append('{')
      var already: Boolean = false
      value.foreach { case (k, v) =>
        if (already) sb.append(',')
        else already = true
        Json.Str(k).writeCompact(sb)
        sb.append(':')
        v.writeCompact(sb)
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
        v.writePretty(sb)
      }
      sb.append(indent)
      sb.append('}')
    }

    override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
      case that: Json.Obj => this.valueMap == that.valueMap
      case _              => false

  }

  case object Null extends Json {

    override private[Json] def writeCompact(sb: mutable.StringBuilder): Unit =
      sb.append("null")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def string(value: String): Json.Str = Json.Str(value)

  def number(value: Byte): Json.Number = Json.Number(BigDecimal(value))
  def number(value: Short): Json.Number = Json.Number(BigDecimal(value))
  def number(value: Int): Json.Number = Json.Number(BigDecimal(value))
  def number(value: Long): Json.Number = Json.Number(BigDecimal(value))
  def number(value: Float): Json.Number = Json.Number(BigDecimal(value))
  def number(value: Double): Json.Number = Json.Number(BigDecimal(value))
  def number(value: BigInt): Json.Number = Json.Number(BigDecimal(value))
  def number(value: BigDecimal): Json.Number = Json.Number(value)

  def boolean(value: Boolean): Json.Bool = Json.Bool(value)

  def arr(value: Json*): Json.Arr = Json.Arr(value.toContiguous)

  def obj(value: (String, Json)*): Json.Obj = Json.Obj(value.toContiguous)

  def parse(string: String): Either[JsonError, Json] =
    JsonParser.parse(string)

  def parseOrJsonString(string: String): Json =
    parse(string).getOrElse(Json.Str(string))

}
