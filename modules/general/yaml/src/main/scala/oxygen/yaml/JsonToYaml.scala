package oxygen.yaml

import org.virtuslab.yaml.*
import oxygen.json.*

object JsonToYaml {

  def node(n: Json): Node =
    n match {
      case Json.Null          => Node.ScalarNode("null")
      case Json.Str(value)    => YamlEncoder.forString.asNode(value)
      case Json.Number(value) => Node.ScalarNode(value.toString)
      case Json.Bool(value)   => Node.ScalarNode(value.toString)
      case Json.Arr(value)    => Node.SequenceNode(value.map(JsonToYaml.node)*)
      case Json.Obj(value)    => Node.MappingNode(value.map { case (k, v) => (YamlEncoder.forString.asNode(k), JsonToYaml.node(v)) }*)
    }

}
