package oxygen.schema


trait SchemaLike[S[_], A] { self: S[A] =>

  val name: String

  val encode: A => String
  val decode: String => Either[String, A]

  // TODO (KR) : these probably need some sort of tag
  def transform[B](ab: A => B, ba: B => A): S[B]
  def transformOrFail[B](ab: A => Either[String, B], ba: B => A): S[B]

  /**
    * This allows you to create a schema which can only [[decode]] values.
    * If the schema produced by one of these functions ever calls [[encode]], it will throw.
    */
  object unsafeDecodeOnly {
    def map[B](ab: A => B): S[B] = transform[B](ab, _ => throw SchemaLike.EncodingNotSupported(name))
    def mapOrFail[B](ab: A => Either[String, B]): S[B] = transformOrFail[B](ab, _ => throw SchemaLike.EncodingNotSupported(name))
  }

  /**
    * This allows you to create a schema which can only [[encode]] values.
    * If the schema produced by one of these functions ever calls [[decode]], it will throw.
    */
  object unsafeEncodeOnly {
    def contramap[B](ba: B => A): S[B] = transform[B](_ => throw SchemaLike.DecodingNotSupported(name), ba)
  }

}
object SchemaLike {

  final case class DecodingNotSupported(name: String) extends Throwable {
    override def getMessage: String =
      s"Attempted to decode value using schema `$name`, which only supports encoding. This is an internal defect caused by the programmer (you cant use `unsafeEncodeOnly` if you want to decode things...)."
  }

  final case class EncodingNotSupported(name: String) extends Throwable {
    override def getMessage: String =
      s"Attempted to encode value using schema `$name`, which only supports decoding. This is an internal defect caused by the programmer (you cant use `unsafeDecodeOnly` if you want to encode things...)."
  }


}
