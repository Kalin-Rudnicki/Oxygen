package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.typeclass.*
import scala.reflect.ClassTag

trait Enum[E <: Enum[E]] { self: E => }
object Enum {

  inline def values[E](implicit c: Companion[E]): Seq[E] = c.enumValues

  // =====|  |=====

  trait Companion[E: TypeTag] { self =>

    given companion: Companion[E] = this

    protected val defaultToString: E => NonEmptyList[String] = e => NonEmptyList.one(e.toString)

    def values: Array[E]

    final lazy val enumValues: Seq[E] = values.toSeq

    abstract class EnumMap[Enc](enc: E => NonEmptyList[Enc]) {
      final lazy val encodedValues: Seq[Enc] = enumValues.flatMap(enc(_).toList)

      protected lazy val map: Map[Enc, E] = values.flatMap { e => enc(e).toList.map((_, e)) }.toMap
      def encode(e: E): Enc = enc(e).head
      def decode(enc: Enc): Option[E] = map.get(enc)
    }

    abstract class CaseInsensitiveStringMap(enc: E => NonEmptyList[String]) extends EnumMap[String](enc) {
      override protected lazy val map: Map[String, E] = values.flatMap { e => enc(e).toList.map(str => (str.toUpperCase, e)) }.toMap
      override def decode(enc: String): Option[E] = map.get(enc.toUpperCase)
    }

    implicit object ToString extends CaseInsensitiveStringMap(defaultToString)

    implicit val stringCodec: StringCodec[E] =
      StringCodec(
        StringEncoder.usingToString[String].contramap(ToString.encode),
        StringDecoder.string.mapOption(ToString.decode),
      )

    final def withDefaultToString(f: E => NonEmptyList[String]): Enum.Companion[E] =
      new Enum.Companion[E] {
        override def values: Array[E] = self.values
        override protected val defaultToString: E => NonEmptyList[String] = f
      }

  }
  object Companion {

    def apply[A: {TypeTag, ClassTag}](_values: A*): Enum.Companion[A] =
      new Companion[A] {
        override def values: Array[A] = _values.toArray
      }

    def fromArray[A: TypeTag](array: Array[A]): Enum.Companion[A] =
      new Companion[A] {
        override def values: Array[A] = array
      }

  }

}
