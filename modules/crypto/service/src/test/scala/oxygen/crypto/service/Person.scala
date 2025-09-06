package oxygen.crypto.service

import oxygen.predef.json.*

final case class Person(
    first: String,
    last: String,
    age: Int,
) derives JsonCodec {
  lazy val jsonString: String = this.toJsonStringCompact
  override def toString: String = jsonString
}
object Person {
  val example1: Person = Person("F1", "L1", 1)
  val example2: Person = Person("F2", "L2", 2)
}
