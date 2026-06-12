package oxygen.json

opaque type SecretValue[A] <: A = A
object SecretValue {
  given [A: JsonEncoder as enc] => JsonEncoder[SecretValue[A]] = enc.secret
}
