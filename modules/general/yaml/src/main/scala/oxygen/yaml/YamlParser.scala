package oxygen.yaml

import scala.collection.mutable

final class YamlParser private (string: String) {

  private object sb {
    private val inner: mutable.StringBuilder = mutable.StringBuilder()

    def append(c: Char): Unit = inner.append(c)
    def append(s: String): Unit = inner.append(s)

    def getAndClear(): String = {
      val str = inner.toString()
      inner.clear()
      str
    }

    override def toString: String = getAndClear()

  }

}
object YamlParser {

  def parse(string: String): Either[String, Yaml] = {
    val parser = new YamlParser(string)

    ??? // FIX-PRE-MERGE (KR) :
  }

}
