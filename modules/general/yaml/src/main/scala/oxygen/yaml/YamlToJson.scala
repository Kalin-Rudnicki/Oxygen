package oxygen.yaml

import org.virtuslab.yaml.*
import oxygen.json.*
import oxygen.predef.core.*

// TODO (KR) : potentially be more safe here?
object YamlToJson {

  def key(n: Node): String =
    n match
      case n: Node.ScalarNode => n.value
      case _                  => throw new RuntimeException(s"Mapping node with non-scalar key: $node")

  def node(n: Node): Json =
    n match {
      case n: Node.ScalarNode =>
        n.tag match {
          case Tag.nullTag => Json.Null
          case Tag.str     =>
            // TODO (KR) : need to figure out what the fuck to do about this stupid yaml behavior....
            val jsonParsedString: Option[String] =
              for {
                pos <- n.pos
                start = pos.start
                end <- pos.end if end.line == start.line
                line = pos.input(start.line)
                exactStr = line.substring(start.column, end.column)
                _ <- Option.when(exactStr.startsWith("\"") && exactStr.endsWith("\""))(())
                parsedStr <- JsonDecoder.string.decodeJsonString(exactStr).toOption
              } yield parsedStr
            Json.string(jsonParsedString.getOrElse(n.value))
          case Tag.int     => Json.number(BigInt(n.value))
          case Tag.float   => Json.number(BigDecimal(n.value))
          case Tag.boolean => Json.boolean(n.value.toBoolean)
          case _           => throw new RuntimeException(s"Unknown tag : ${n.tag.value} : ${n.value}")
        }
      case n: Node.SequenceNode => Json.Arr(ArraySeq.from(n.nodes).map(YamlToJson.node))
      case n: Node.MappingNode  =>
        Json.Obj(
          ArraySeq.from(n.mappings).sortBy(_._1.pos.fold(0)(_.start.offset)).map { case (k, v) =>
            (
              YamlToJson.key(k),
              YamlToJson.node(v),
            )
          },
        )
    }

}
