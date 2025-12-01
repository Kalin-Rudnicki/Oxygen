package oxygen.json

import oxygen.predef.core.*
import scala.annotation.tailrec
import scala.collection.mutable

private[json] final class JsonParser private (string: String) {
  private var idx: Int = 0

  private object sb {
    private val inner: mutable.StringBuilder = mutable.StringBuilder()

    def append(c: Char): Unit = inner.append(c)
    def append(s: String): Unit = inner.append(s)

    def getAndClear(): String = {
      val str = inner.toString()
      inner.clear()
      str
    }

  }

  private inline def fail(): Nothing =
    throw new JsonError(Nil, JsonError.Cause.InvalidJson(idx, None))

  private inline def fail(errMsg: String): Nothing =
    throw new JsonError(Nil, JsonError.Cause.InvalidJson(idx, new RuntimeException(errMsg).some))

  private inline def expectChar(char: Char): Unit = {
    val actual = string(idx)
    if actual == char then idx += 1
    else fail(s"Expected char ${char.unesc}, but got: ${actual.unesc}")
  }

  /**
    * Will skip white space, not caring to watch out for array bounds.
    */
  private inline def skipWhiteSpace(): Unit =
    while string(idx).isWhitespace do idx += 1

  /**
    * Will skip white space, caring to watch out for array bounds.
    */
  private inline def safeSkipWhiteSpace(): Unit =
    while idx < string.length && string(idx).isWhitespace do idx += 1

  private inline def assertEOF(): Unit =
    if idx != string.length then fail("Expected EOF, but is not")

  private def parseRemainingString(): String = {

    @tailrec
    def continue(): Unit = {
      if idx >= string.length then fail("JSON body terminated before end of string")

      string(idx) match {
        case '"' =>
          idx += 1
        case '\\' =>
          string(idx + 1) match {
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
    }

    continue()

    sb.getAndClear()
  }

  // TODO (KR) : this is going to break if parsing a root json
  private def parseRemainingNumber(init: String): Json.Number = {
    sb.append(init)

    @tailrec
    def continue(): Unit =
      string.lift(idx) match {
        case Some(c) if c >= '0' && c <= '9' =>
          sb.append(c)
          idx += 1
          continue()
        case _ =>
          ()
      }

    continue()

    string.lift(idx) match {
      case Some('.') =>
        string.lift(idx + 1) match {
          case Some(c) if c >= '0' && c <= '9' =>
            sb.append('.')
            sb.append(c)
            idx += 2
            continue()
            Json.Number(BigDecimal(sb.getAndClear()))
          case _ =>
            fail()
        }
      case _ =>
        Json.Number(BigDecimal(sb.getAndClear()))
    }
  }

  private def parseObjectPair(): (String, Json) = {
    expectChar('"')
    val key = parseRemainingString()
    skipWhiteSpace()
    expectChar(':')
    skipWhiteSpace()
    val json = parseAnyJson()
    (key, json)
  }

  private def parseAnyJson(): Json =
    string(idx) match {
      case '"' =>
        idx += 1
        Json.Str(parseRemainingString())
      case 't' =>
        idx += 1
        expectChar('r')
        expectChar('u')
        expectChar('e')
        Json.Bool(true)
      case 'f' =>
        idx += 1
        expectChar('a')
        expectChar('l')
        expectChar('s')
        expectChar('e')
        Json.Bool(false)
      case 'n' =>
        idx += 1
        expectChar('u')
        expectChar('l')
        expectChar('l')
        Json.Null
      case '{' =>
        val builder = ArraySeq.newBuilder[(String, Json)]
        @tailrec
        def loop(): Json.Obj = {
          skipWhiteSpace()
          string(idx) match {
            case ',' =>
              idx += 1
              skipWhiteSpace()
              builder.addOne(parseObjectPair())
              loop()
            case '}' =>
              idx += 1
              Json.Obj(builder.result())
            case _ =>
              fail()
          }
        }

        idx += 1
        skipWhiteSpace()
        string(idx) match {
          case '"' =>
            builder.addOne(parseObjectPair())
            loop()
          case '}' =>
            idx += 1
            Json.Obj(ArraySeq.empty)
          case _ =>
            fail()
        }
      case '[' =>
        val builder = ArraySeq.newBuilder[Json]
        @tailrec
        def loop(): Json.Arr = {
          skipWhiteSpace()
          string(idx) match {
            case ',' =>
              idx += 1
              skipWhiteSpace()
              builder.addOne(parseAnyJson())
              loop()
            case ']' =>
              idx += 1
              Json.Arr(builder.result())
            case _ =>
              fail()
          }
        }

        idx += 1
        skipWhiteSpace()
        string(idx) match {
          case ']' =>
            idx += 1
            Json.Arr(ArraySeq.empty)
          case _ =>
            builder.addOne(parseAnyJson())
            loop()
        }
      case '-' =>
        string(idx + 1) match {
          case c if c >= '0' && c <= '9' =>
            idx += 2
            parseRemainingNumber(new String(Array('-', c)))
          case _ =>
            fail()
        }
      case c if c >= '0' && c <= '9' =>
        idx += 1
        parseRemainingNumber(new String(Array(c)))
      case _ =>
        fail()
    }

}
object JsonParser {

  def parse(string: String): Either[JsonError, Json] =
    try {
      val parser = new JsonParser(string)

      parser.skipWhiteSpace()

      val json = parser.parseAnyJson()

      parser.safeSkipWhiteSpace()
      parser.assertEOF()

      json.asRight
    } catch {
      case jsonError: JsonError         => jsonError.asLeft
      case _: IndexOutOfBoundsException => JsonError(Nil, JsonError.Cause.InvalidJson(string.length, None)).asLeft
      case e: Throwable                 => JsonError(Nil, JsonError.Cause.InvalidJson(-1, e.some)).asLeft
    }

}
