package oxygen.schema

final case class Base64[A](value: A)
object Base64 {
  final case class Url[A](value: A)
  final case class Mime[A](value: A)
}
