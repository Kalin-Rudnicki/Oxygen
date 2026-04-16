package oxygen.yaml

import org.virtuslab.yaml.*
import oxygen.json.*

object YamlWriter {

  def writeJson(n: Json): String = JsonToYaml.node(n).asYaml

  def writeManyJson(n: Seq[Json]): String = n.map(YamlWriter.writeJson).mkString("\n---\n")

  def writeJsonOf[A: JsonEncoder as enc](value: A): String = YamlWriter.writeJson(enc.encodeJsonAST(value))
  def writeManyJsonOf[A: JsonEncoder as enc](values: Seq[A]): String = YamlWriter.writeManyJson(values.map(enc.encodeJsonAST))
  def writeManyJsonOfWithHeader[H: JsonEncoder as hEnc, Elem: JsonEncoder as elemEnc](header: H, elems: Seq[Elem]): String = {
    val sb = scala.collection.mutable.StringBuilder()
    sb.append(writeJsonOf[H](header))
    elems.foreach { elem =>
      sb.append("\n---\n")
      sb.append(writeJsonOf[Elem](elem))
    }
    sb.toString()
  }

}
