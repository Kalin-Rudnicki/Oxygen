package oxygen.schema

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

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

  def `null`: Json.Null.type = Json.Null

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Parser
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def parseInternal(string: String): Option[Json] =
    Try {
      val arr: IArray[Char] = IArray.unsafeFromArray(string.toCharArray)
      var idx: Int = 0

      inline def fail(): Nothing = {
        println((idx, arr.lift(idx)))
        throw new RuntimeException
      }

      def skipWhiteSpace(): Unit =
        while (arr(idx).isWhitespace)
          idx += 1

      def safeSkipWhiteSpace(): Unit =
        while (idx < arr.length && arr(idx).isWhitespace)
          idx += 1

      def parseRemainingString(): String = {
        val sb = mutable.StringBuilder()

        @tailrec
        def continue(): Unit =
          arr(idx) match {
            case '"' =>
              idx += 1
            case '\\' =>
              arr(idx + 1) match {
                case '"' => sb.append('"')
                case 'n' => sb.append('\n')
                case 't' => sb.append('\t')
                case c   => sb.append(c)
              }
              idx += 2
              continue()
            case c =>
              sb.append(c)
              idx += 1
              continue()
          }

        continue()

        sb.toString()
      }

      // TODO (KR) : this is going to break if the root json is a number...
      def parseRemainingNumber(init: String): Json.Number = {
        val sb = mutable.StringBuilder(init)

        @tailrec
        def continue(): Unit =
          arr(idx) match {
            case c if c >= '0' && c <= '9' =>
              sb.append(c)
              idx += 1
              continue()
            case _ =>
              ()
          }

        continue()

        arr(idx) match {
          case '.' =>
            arr(idx + 1) match {
              case c if c >= '0' && c <= '9' =>
                sb.append('.')
                sb.append(c)
                idx += 2
                continue()
                Json.NumberWithDecimal(BigDecimal(sb.toString))
              case _ =>
                fail()
            }
          case _ =>
            Json.NumberWithoutDecimal(BigInt(sb.toString))
        }
      }

      @tailrec
      def parseRemainingObject(rParsed: List[(String, Json)]): Json.Obj = {
        skipWhiteSpace()
        arr(idx) match {
          case ',' =>
            idx += 1
            skipWhiteSpace()
            expectChar('"')
            val key = parseRemainingString()
            skipWhiteSpace()
            expectChar(':')
            skipWhiteSpace()
            val json = parseAnyJson()
            parseRemainingObject((key, json) :: rParsed)
          case '}' =>
            idx += 1
            Json.Obj(IArray.from(rParsed.reverse))
          case _ =>
            fail()
        }
      }

      @tailrec
      def parseRemainingArray(rParsed: List[Json]): Json.Arr = {
        skipWhiteSpace()
        arr(idx) match {
          case ',' =>
            idx += 1
            skipWhiteSpace()
            val json = parseAnyJson()
            parseRemainingArray(json :: rParsed)
          case ']' =>
            idx += 1
            Json.Arr(IArray.from(rParsed.reverse))
          case _ =>
            fail()
        }
      }

      def expectChar(char: Char): Unit =
        if (arr(idx) == char) idx += 1
        else fail()

      def expectString(string: String): Unit =
        string.foreach(expectChar)

      def parseAnyJson(): Json =
        arr(idx) match {
          case '"' =>
            idx += 1
            Json.Str(parseRemainingString())
          case 't' =>
            idx += 1
            expectString("rue")
            Json.Bool(true)
          case 'f' =>
            idx += 1
            expectString("alse")
            Json.Bool(false)
          case 'n' =>
            idx += 1
            expectString("ull")
            Json.Null
          case '{' =>
            idx += 1
            skipWhiteSpace()
            arr(idx) match {
              case '"' =>
                idx += 1
                val name = parseRemainingString()
                skipWhiteSpace()
                expectChar(':')
                skipWhiteSpace()
                val json = parseAnyJson()
                parseRemainingObject((name, json) :: Nil)
              case '}' =>
                idx += 1
                Json.Obj(IArray.empty)
              case _ =>
                fail()
            }
          case '[' =>
            idx += 1
            skipWhiteSpace()
            arr(idx) match {
              case ']' =>
                idx += 1
                Json.Arr(IArray.empty)
              case _ =>
                val json = parseAnyJson()
                parseRemainingArray(json :: Nil)
            }
          case '-' =>
            arr(idx + 1) match {
              case c if c >= '0' && c <= '9' =>
                idx += 2
                parseRemainingNumber(new String(Array('-', c)))
              case _ =>
                fail()
            }
          case c if c >= '0' && c <= '9' =>
            idx += 1
            parseRemainingNumber(c.toString)
          case _ =>
            fail()
        }

      skipWhiteSpace()
      val json = parseAnyJson()
      safeSkipWhiteSpace()

      if (idx != arr.length)
        fail()

      json
    }.toOption

  def parse(string: String): Either[JsonError, Json] =
    parseInternal(string).toRight(JsonError(Nil, JsonError.Cause.DecodingFailed("Invalid Json")))

}

// TODO (KR) :
object TmpMain extends scala.App {

  def commas(long: Long): String = {
    @tailrec
    def loop(rQueue: List[Char], stack: List[Char]): String =
      rQueue match {
        case c :: b :: a :: Nil  => (a :: b :: c :: stack).mkString
        case c :: b :: a :: rest => loop(rest, ',' :: a :: b :: c :: stack)
        case _                   => (rQueue.reverse ::: stack).mkString
      }

    val (prefix, rest) =
      long.toString.toList match {
        case '-' :: rest => "-" -> rest
        case rest        => "" -> rest
      }

    rest.reverse match {
      case c :: b :: a :: rest => prefix + loop(rest, ',' :: a :: b :: c :: Nil)
      case _                   => prefix + rest.mkString
    }
  }

  def timeThatShit[A](label: String)(eff: => A): A = {
    val start = System.nanoTime()
    val x = eff
    val end = System.nanoTime()
    println(s"nanos ($label) : ${commas(end - start)}")
    x
  }

  val json0 =
    Json.arr(
      Json.number(1),
      Json.Str("abc"),
      Json.Bool(true),
      Json.obj(),
      Json.obj(
        "k" -> Json.string("v"),
        "a" -> Json.arr(),
        "o" -> Json.obj(),
        "o2" -> Json.obj("b" -> Json.boolean(false)),
        "\"\ns" -> Json.Null,
      ),
    )

  val json = timeThatShit("make") {
    Json.arr(
      Json.number(1),
      Json.Str("abc"),
      Json.Bool(true),
      Json.obj(),
      Json.obj(
        "k" -> Json.string("v"),
        "a" -> Json.arr(),
        "o" -> Json.obj(),
        "o2" -> Json.obj("b" -> Json.boolean(false)),
        "\"\ns" -> Json.Null,
      ),
    )
  }

  json.showSimple
  json.showPretty

  println(timeThatShit("show - simple")(json.showSimple))
  println(timeThatShit("show - pretty")(json.showPretty))

  Json.parse("{}")
  Json.parse("[]")
  Json.parse("\"\"")
  
  println(
    timeThatShit("parse") {
      Json
        .parseInternal(
          s"""{
             |  "a": "b",
             |  "c": null,
             |  "d": [
             |    {},
             |    false,
             |    true,
             |    1,
             |    -5,
             |    17.56,
             |    -56.24
             |  ]
             |}""".stripMargin,
        )
    }.get,
  )

}
