package oxygen.yaml

import scala.collection.mutable

final class YamlBuilder private () {

  private val sb: mutable.StringBuilder = mutable.StringBuilder()

  private def write(indent: Int, indentString: String, yaml: Yaml): Unit =
    ??? // FIX-PRE-MERGE (KR) :

}
object YamlBuilder {

  def format(yaml: Yaml): String = {
    val builder = new YamlBuilder()
    builder.write(0, "  ", yaml)
    builder.sb.toString()
  }

}
