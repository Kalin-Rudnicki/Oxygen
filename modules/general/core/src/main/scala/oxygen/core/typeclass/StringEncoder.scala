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

  final def mapString(that: StringEncoder[String]): StringEncoder[A] =
    new StringEncoder[A] {
      override def encode(value: A): String = that.encode(self.encode(value))
    }

}
object StringEncoder {

  inline def apply[A](implicit ev: StringEncoder[A]): ev.type = ev

  implicit def fromCodec[A: StringCodec]: StringDecoder[A] = StringCodec[A].decoder

  // =====|  |=====

  def usingToString[A]: StringEncoder[A] = _.toString

  val string: StringEncoder[String] =
    new StringEncoder[String] {
      override def encode(value: String): String = value
    }

}
