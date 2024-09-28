package oxygen.core

import oxygen.core.typeclass.*

trait Enum[E <: Enum[E]] { self: E => }
object Enum {

  inline def values[E <: Enum[E]](implicit hc: HasCompanion[E]): Seq[E] = hc.companion.enumValues

  // =====|  |=====

  final case class HasCompanion[E <: Enum[E]] private[Enum] (companion: Companion[E])

  trait Companion[E <: Enum[E]: TypeTag] {

    implicit final val hasCompanion: HasCompanion[E] = Enum.HasCompanion(this)

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
        StringEncoder.usingToString[String].cmap(ToString.encode),
        StringDecoder.string.mapOption(ToString.decode),
      )

  }

}
