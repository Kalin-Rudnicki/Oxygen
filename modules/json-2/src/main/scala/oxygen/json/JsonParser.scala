package oxygen.json

import oxygen.predef.core.*
import scala.annotation.tailrec
import scala.collection.mutable

private[json] final class JsonParser private (string: String) {
  private val arr: IArray[Char] = IArray.unsafeFromArray(string.toCharArray)
  private var idx: Int = 0

  private inline def fail(): Nothing =
    throw new JsonError(Nil, JsonError.Cause.InvalidJson(idx))

  private inline def expectChar(char: Char): Unit =
    if (arr(idx) == char) idx += 1
    else fail()

  /**
    * Will skip white space, not caring to watch out for array bounds.
    */
  private inline def skipWhiteSpace(): Unit =
    while (arr(idx).isWhitespace)
      idx += 1

  /**
    * Will skip white space, caring to watch out for array bounds.
    */
  private inline def safeSkipWhiteSpace(): Unit =
    while (idx < arr.length && arr(idx).isWhitespace)
      idx += 1

  private inline def assertEOF(): Unit =
    if (idx != arr.length)
      fail()

  private def parseRemainingString(): String = {
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

  // TODO (KR) : this is going to break if parsing a root json
  private def parseRemainingNumber(init: String): Json.Number = {
    val sb = mutable.StringBuilder(init)

    @tailrec
    def continue(): Unit =
      arr.lift(idx) match {
        case Some(c) if c >= '0' && c <= '9' =>
          sb.append(c)
          idx += 1
          continue()
        case _ =>
          ()
      }

    continue()

    arr.lift(idx) match {
      case Some('.') =>
        arr.lift(idx + 1) match {
          case Some(c) if c >= '0' && c <= '9' =>
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
    arr(idx) match {
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
        val builder = IArray.newBuilder[(String, Json)]
        @tailrec
        def loop(): Json.Obj = {
          skipWhiteSpace()
          arr(idx) match {
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
        arr(idx) match {
          case '"' =>
            builder.addOne(parseObjectPair())
            loop()
          case '}' =>
            idx += 1
            Json.Obj(IArray.empty)
          case _ =>
            fail()
        }
      case '[' =>
        val builder = IArray.newBuilder[Json]
        @tailrec
        def loop(): Json.Arr = {
          skipWhiteSpace()
          arr(idx) match {
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
        arr(idx) match {
          case ']' =>
            idx += 1
            Json.Arr(IArray.empty)
          case _ =>
            builder.addOne(parseAnyJson())
            loop()
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
      case jsonError: JsonError => jsonError.asLeft
      case _: Throwable         => JsonError(Nil, JsonError.Cause.InvalidJson(-1)).asLeft
    }

}
