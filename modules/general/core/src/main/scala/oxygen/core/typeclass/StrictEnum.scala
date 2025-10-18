package oxygen.core.typeclass

import oxygen.core.TypeTag
import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.string.*
import scala.annotation.targetName

trait StrictEnum[A] {
  val typeTag: TypeTag[A]
  val enumValues: Seq[A]
  def encodeMany(value: A): NonEmptyList[String]
  final def encode(value: A): String = encodeMany(value).head

  private lazy val encodedPairs: Seq[(String, A)] = enumValues.flatMap(a => encodeMany(a).toList.map((_, a)))
  final lazy val encodedValues: Seq[String] = encodedPairs.map(_._1)
  final lazy val encodedLowerCaseMap: Map[String, A] = encodedPairs.map { case (k, v) => (k.toLowerCase, v) }.toMap

  final lazy val valuesString: String = enumValues.mkString("[", ", ", "]")
  final lazy val encodedValuesString: String = encodedValues.map(_.unesc).mkString("[", ", ", "]")

  final def decodeOption(value: String): Option[A] = encodedLowerCaseMap.get(value.toLowerCase)
  final def decodeEither(value: String): Either[String, A] = decodeOption(value).toRight(s"Invalid ${typeTag.prefixObject} (${value.unesc})")
  final def decodeEitherWithHint(value: String): Either[String, A] = decodeOption(value).toRight(s"Invalid ${typeTag.prefixObject} (${value.unesc}), valid options: $encodedValuesString")

  final def unapply(value: String): Option[A] = decodeOption(value)

  override def toString: String = s"StrictEnum[${typeTag.prefixObject}](values = $valuesString, encodedValues = $encodedValuesString)"

}
object StrictEnum {

  def apply[A: StrictEnum as e]: StrictEnum[A] = e

  final case class Impl[A](typeTag: TypeTag[A], enumValues: Seq[A], enc: A => NonEmptyList[String]) extends StrictEnum[A] {
    override def encodeMany(value: A): NonEmptyList[String] = enc(value)
  }

  @targetName("make_values_nel")
  def make[A: TypeTag as tt](values: Seq[A], encode: A => NonEmptyList[String]): StrictEnum[A] = StrictEnum.Impl(tt, values, encode)

  @targetName("make_values_single")
  def make[A: TypeTag](values: Seq[A], encode: A => String): StrictEnum[A] = make[A](values, a => NonEmptyList.one(encode(a)))

  @targetName("make_values")
  def make[A: {TypeTag, EnumEncoding as enc}](values: Seq[A]): StrictEnum[A] = make[A](values, enc.encodeMany)

}
