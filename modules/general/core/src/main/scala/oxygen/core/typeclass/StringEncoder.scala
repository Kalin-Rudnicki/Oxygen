package oxygen.core.typeclass

trait StringEncoder[A] { self =>

  def encode(value: A): String

  /**
    * Contra-map the input to this StringEncoder.
    */
  final def contramap[B](f: B => A): StringEncoder[B] =
    new StringEncoder[B] {
      override def encode(value: B): String = self.encode(f(value))
    }

  final def mapOutputString(that: StringEncoder[String]): StringEncoder[A] = StringEncoder.MapOutputString(this, that)

}
object StringEncoder extends StringEncoderLowPriority.LowPriority1 {

  inline def apply[A](implicit ev: StringEncoder[A]): ev.type = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: StringEncoder[A])
    def >>>(that: StringEncoder[String]): StringEncoder[A] =
      StringEncoder.MapOutputString(self, that)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Identity extends StringEncoder[String] {
    override def encode(value: String): String = value
  }

  final class UsingToString[A] extends StringEncoder[A] {
    override def encode(value: A): String = value.toString
  }

  final case class MapOutputString[A](base: StringEncoder[A], out: StringEncoder[String]) extends StringEncoder[A] {
    override def encode(value: A): String = out.encode(base.encode(value))
  }

  val string: StringEncoder[String] = Identity

  def usingToString[A]: StringEncoder[A] = new UsingToString[A]

}

object StringEncoderLowPriority {

  trait LowPriority1 {

    given fromCodec: [A: StringCodec as codec] => StringEncoder[A] = codec.encoder

  }

}
