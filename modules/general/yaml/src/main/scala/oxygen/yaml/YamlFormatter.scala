package oxygen.yaml

import oxygen.core.StringBuilder
import oxygen.core.collection.mutable.ArrayBuilder
import oxygen.json.Json

object YamlFormatter {

  def format(
      json: Json,
      config: YamlFormatter.Config = YamlFormatter.Config(),
  ): String = {
    val sb = StringBuilder.emptyThreadUnsafe
    formatInto(json, sb, config)
    sb.build()
  }

  def formatInto(
      json: Json,
      sb: StringBuilder,
      config: YamlFormatter.Config = YamlFormatter.Config(),
  ): Unit =
    formatIntoInternalRoot(
      json = json,
      sb = sb,
      config = config,
    )

  final case class Config(
      indentSize: Int = 2,
  ) {
    if indentSize < 2 then
      throw new RuntimeException(s"Invalid YamlFormatter.Config.indentSize($indentSize), must be >= 2")

    val spaceIndentString: String = " " * indentSize
    val continueArrayIndentString: String = spaceIndentString.drop(2) + ArrayMarker

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Width of a single `- ` array marker (always 2). Continuation lines use the same width. */
  private val ArrayMarker: String = "- "
  private val ArrayMarkerPad: String = "  " // same width as Marker, spaces only

  private inline def writeCharNTimes(n: Int, char: Char, sb: StringBuilder): Unit =
    n match {
      case 0 => ()
      case 1 => sb.append(char)
      case _ =>
        var c: Int = 0
        while c < n do {
          sb.append(char)
          c = c + 1
        }
    }

  private inline def writeStringNTimes(n: Int, str: String, sb: StringBuilder): Unit =
    n match {
      case 0 => ()
      case 1 => sb.append(str)
      case _ =>
        var c: Int = 0
        while c < n do {
          sb.append(str)
          c = c + 1
        }
    }

  private def formatIntoInternalRoot(
      json: Json,
      sb: StringBuilder,
      config: YamlFormatter.Config,
  ): Unit =
    JsonCategory.from(json) match {
      case value: JsonCategory.NonInline =>
        val builder: ArrayBuilder[Line] = ArrayBuilder.emptyThreadUnsafe
        formatIntoInternalNonRoot(value, builder, 0, 0, 0)

        val resultLines: Array[Line] = builder.buildArray()
        val numLines: Int = resultLines.length

        var idx: Int = 0
        while idx < numLines do {
          val currentLine: Line = resultLines(idx)

          if currentLine.blankSpaces == 0 && idx > 0 then // add a single space between top level keys
            sb.append('\n')

          sb.append('\n')

          writeStringNTimes(currentLine.blankSpaces, config.spaceIndentString, sb)
          writeStringNTimes(currentLine.blankArrays, ArrayMarkerPad, sb)

          currentLine.arrays match {
            case 0         => ()
            case numArrays =>
              sb.append(ArrayMarker)
              writeStringNTimes(numArrays - 1, config.continueArrayIndentString, sb)
          }

          currentLine match {
            case line: Line.ObjectKeyInline =>
              writeKeySafe(line.key, sb)
              sb.append(": ")
              line.value.writeCompact(sb)
            case line: Line.ObjectKeyNonInline =>
              writeKeySafe(line.key, sb)
              sb.append(':')
            case line: Line.ArrayElemInline =>
              line.value.writeCompact(sb)
          }

          idx += 1
        }

        sb.append('\n')

      case JsonCategory.Inline(json) =>
        sb.append('\n')
        json.writeCompact(sb)
        sb.append('\n')
    }

  private def formatIntoInternalNonRoot(
      json: JsonCategory.NonInline,
      builder: ArrayBuilder[Line],
      blankSpaces: Int,
      blankArrays: Int,
      arrays: Int,
  ): Unit =
    json match {
      case JsonCategory.Obj(Json.Obj(values)) =>
        val numValues: Int = values.length

        val (childNBlankArrays, childNBlankSpaces): (Int, Int) =
          if arrays == 0 then (blankArrays, blankSpaces + 1)
          else (blankArrays + 1, blankSpaces + arrays)
        val nestedBlankSpaces: Int = childNBlankSpaces - 1

        var idx: Int = 0
        JsonCategory.from(values(idx)) match {
          case (key, JsonCategory.Inline(value)) =>
            builder.addSingle(YamlFormatter.Line.ObjectKeyInline(blankSpaces, blankArrays, arrays, key, value))
          case (key, value: JsonCategory.NonInline) =>
            builder.addSingle(YamlFormatter.Line.ObjectKeyNonInline(blankSpaces, blankArrays, arrays, key))
            formatIntoInternalNonRoot(value, builder, childNBlankSpaces, childNBlankArrays, 0)
        }
        idx += 1

        while idx < numValues do {
          JsonCategory.from(values(idx)) match {
            case (key, JsonCategory.Inline(value)) =>
              builder.addSingle(YamlFormatter.Line.ObjectKeyInline(nestedBlankSpaces, childNBlankArrays, 0, key, value))
            case (key, value: JsonCategory.NonInline) =>
              builder.addSingle(YamlFormatter.Line.ObjectKeyNonInline(nestedBlankSpaces, childNBlankArrays, 0, key))
              formatIntoInternalNonRoot(value, builder, childNBlankSpaces, childNBlankArrays, 0)
          }

          idx += 1
        }

      case JsonCategory.Arr(Json.Arr(values)) =>
        val numValues: Int = values.length
        val currentArrays: Int = arrays + 1
        val childNBlankSpaces: Int = blankSpaces + arrays

        var idx: Int = 0
        JsonCategory.from(values(idx)) match {
          case JsonCategory.Inline(value) =>
            builder.addSingle(YamlFormatter.Line.ArrayElemInline(blankSpaces, blankArrays, currentArrays, value))
          case value: JsonCategory.NonInline =>
            formatIntoInternalNonRoot(value, builder, blankSpaces, blankArrays, currentArrays)
        }
        idx += 1

        while idx < numValues do {
          JsonCategory.from(values(idx)) match {
            case JsonCategory.Inline(value) =>
              builder.addSingle(YamlFormatter.Line.ArrayElemInline(childNBlankSpaces, blankArrays, 1, value))
            case value: JsonCategory.NonInline =>
              formatIntoInternalNonRoot(value, builder, childNBlankSpaces, blankArrays, 1)
          }

          idx += 1
        }
    }

  private inline def writeKeySafe(key: String, sb: StringBuilder): Unit =
    // Unquoted when non-empty, only [A-Za-z0-9_-], and does not start with a digit.
    if key.nonEmpty && key.forall(safeChars.contains) && !key.head.isDigit then
      sb.append(key)
    else
      Json.Str(key).writeCompact(sb)

  private val safeChars: Set[Char] =
    Seq[IterableOnce[Char]](
      'a'.to('z'),
      'A'.to('Z'),
      Seq('-', '_'),
      '0'.to('9'),
    ).flatten.toSet

  private sealed trait JsonCategory {
    val json: Json
  }
  private object JsonCategory {

    sealed trait NonInline extends JsonCategory

    final case class Inline(json: Json) extends JsonCategory
    final case class Obj(json: Json.Obj) extends JsonCategory.NonInline
    final case class Arr(json: Json.Arr) extends JsonCategory.NonInline

    def from(json: Json): JsonCategory = json match
      case json: Json.Obj if json.value.nonEmpty => JsonCategory.Obj(json)
      case json: Json.Arr if json.value.nonEmpty => JsonCategory.Arr(json)
      case _                                     => JsonCategory.Inline(json)

    def from(pair: (String, Json)): (String, JsonCategory) =
      (pair._1, from(pair._2))

  }

  private sealed trait Line {
    val blankSpaces: Int
    val blankArrays: Int
    val arrays: Int
  }
  private object Line {

    final case class ObjectKeyInline(
        blankSpaces: Int,
        blankArrays: Int,
        arrays: Int,
        key: String,
        value: Json,
    ) extends Line

    final case class ObjectKeyNonInline(
        blankSpaces: Int,
        blankArrays: Int,
        arrays: Int,
        key: String,
    ) extends Line

    final case class ArrayElemInline(
        blankSpaces: Int,
        blankArrays: Int,
        arrays: Int,
        value: Json,
    ) extends Line

  }

}
