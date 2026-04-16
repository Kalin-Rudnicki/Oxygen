package oxygen.yaml

import org.virtuslab.yaml.*
import oxygen.json.*
import oxygen.predef.core.*

object YamlParser {

  def parseJson(yaml: String): Either[String, Json] =
    parseYaml(yaml).bimap(_.safeGetMessage, YamlToJson.node)

  def parseManyJson(yaml: String): Either[String, List[Json]] = {
    val split: List[String] = yaml.split("\n---\n").toList.map(_.trim).filter(_.nonEmpty)
    split.traverse(parseJson)
  }

  def parseJsonOf[A: JsonDecoder as dec](yaml: String): Either[String, A] =
    YamlParser.parseJson(yaml).flatMap(dec.decodeJsonAST(_).leftMap(_.safeGetMessage))

  def parseManyJsonOf[A: JsonDecoder as dec](yaml: String): Either[String, List[A]] =
    YamlParser.parseManyJson(yaml).flatMap(_.traverse(dec.decodeJsonAST(_).leftMap(_.safeGetMessage)))

  def parseManyJsonOfWithHeader[H: JsonDecoder as hDec, Elem: JsonDecoder as elemDec](yaml: String): Either[String, (H, List[Elem])] =
    for {
      jsons <- YamlParser.parseManyJson(yaml)
      (headerJson, elemJsons) <- jsons match
        case head :: tail => (head, tail).asRight
        case Nil          => "Can not parse header from empty document list".asLeft
      parsedHeader <- hDec.decodeJsonAST(headerJson).leftMap(_.safeGetMessage)
      parsedElems <- elemJsons.traverse(elemDec.decodeJsonAST(_).leftMap(_.safeGetMessage))
    } yield (parsedHeader, parsedElems)

}
