package oxygen.core.model

import java.util.UUID

opaque type StringNewType <: String = String
object StringNewType {
  def apply(value: String): StringNewType = value
}

opaque type UUIDNewType <: UUID = UUID
object UUIDNewType {
  def apply(value: UUID): UUIDNewType = value
}
