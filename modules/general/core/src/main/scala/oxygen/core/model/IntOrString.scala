package oxygen.core.model

import oxygen.core.typeclass.StringCodec

opaque type IntOrString <: Int | String = Int | String
object IntOrString {

  def fromString(str: String): IntOrString =
    str.toIntOption.getOrElse(str)

  def apply(ios: Int | String): IntOrString = ios
  extension (self: IntOrString) def unwrap: Int | String = self

  given StringCodec[IntOrString] = StringCodec.string.transform(fromString, _.toString)

}
