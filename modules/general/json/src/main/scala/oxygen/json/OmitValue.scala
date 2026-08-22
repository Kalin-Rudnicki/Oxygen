package oxygen.json

/**
  * Wrapper type carrying "omit from JSON" semantics, parallel to [[SecretValue]].
  *
  * A field typed `OmitValue[A]` is never written to an enclosing JSON object (the derived
  * [[JsonEncoder]] uses `addToObject = false`). On decode the field is reconstructed from its
  * constructor default (or, for types like `Option`/`Specified`, from their `onMissingFromObject`).
  *
  * Example:
  * {{{
  *   final case class User(
  *       name: String,
  *       cachedHash: OmitValue[String] = OmitValue(""),
  *   ) derives JsonCodec
  * }}}
  */
opaque type OmitValue[A] <: A = A
object OmitValue {

  def apply[A](value: A): OmitValue[A] = value

  extension [A](self: OmitValue[A]) def value: A = self

  given encoder: [A: JsonEncoder as enc] => JsonEncoder[OmitValue[A]] = enc.omit
  given decoder: [A: JsonDecoder as dec] => JsonDecoder[OmitValue[A]] = dec

}
