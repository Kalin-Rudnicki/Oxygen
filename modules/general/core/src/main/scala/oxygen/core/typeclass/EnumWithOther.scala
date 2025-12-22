package oxygen.core.typeclass

import oxygen.core.TypeTag
import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.string.*
import oxygen.meta.k0.*
import scala.annotation.targetName

trait EnumWithOther[A] {
  val typeTag: TypeTag[A]
  val enumValues: Seq[A]
  def encodeMany(value: A): NonEmptyList[String]
  def wrapOther(value: String): A
  final def encode(value: A): String = encodeMany(value).head

  private lazy val encodedPairs: Seq[(String, A)] = enumValues.flatMap(a => encodeMany(a).toList.map((_, a)))
  final lazy val encodedValues: Seq[String] = encodedPairs.map(_._1)
  final lazy val encodedLowerCaseMap: Map[String, A] = encodedPairs.map { case (k, v) => (k.toLowerCase, v) }.toMap

  final lazy val valuesString: String = enumValues.mkString("[", ", ", "]")
  final lazy val encodedValuesString: String = encodedValues.map(_.unesc).mkString("[", ", ", "]")

  final def decode(value: String): A = encodedLowerCaseMap.getOrElse(value.toLowerCase, wrapOther(value))

  final def unapply(value: String): Some[A] = Some(decode(value))

  override def toString: String = s"EnumWithOther[${typeTag.prefixObject}](values = $valuesString, encodedValues = $encodedValuesString)"

}
object EnumWithOther {

  def apply[A: EnumWithOther as e]: EnumWithOther[A] = e

  final case class Impl[A](typeTag: TypeTag[A], enumValues: Seq[A], enc: A => NonEmptyList[String], wrap: String => A) extends EnumWithOther[A] {
    override def encodeMany(value: A): NonEmptyList[String] = enc(value)
    override def wrapOther(value: String): A = wrap(value)
  }

  trait WrapOther[A] {
    def wrapOther(value: String): A
  }

  @targetName("make_values_nel_wrap")
  def make[A: TypeTag as tt](values: Seq[A], encode: A => NonEmptyList[String], wrapOther: String => A): EnumWithOther[A] = EnumWithOther.Impl(tt, values, encode, wrapOther)

  @targetName("make_values_single_wrap")
  def make[A: TypeTag](values: Seq[A], encode: A => String, wrapOther: String => A): EnumWithOther[A] = make[A](values, a => NonEmptyList.one(encode(a)), wrapOther)

  @targetName("make_values_wrap")
  def make[A: {TypeTag, EnumEncoding as enc}](values: Seq[A], wrapOther: String => A): EnumWithOther[A] = make[A](values, enc.encodeMany, wrapOther)

  @targetName("make_values_nel")
  def make[A: {TypeTag, EnumWithOther.WrapOther as w}](values: Seq[A], encode: A => NonEmptyList[String]): EnumWithOther[A] = make[A](values, encode, w.wrapOther)

  @targetName("make_values_single")
  def make[A: {TypeTag, EnumWithOther.WrapOther as w}](values: Seq[A], encode: A => String): EnumWithOther[A] = make[A](values, encode, w.wrapOther)

  @targetName("make_values")
  def make[A: {TypeTag, EnumEncoding, EnumWithOther.WrapOther as w}](values: Seq[A]): EnumWithOther[A] = make[A](values, w.wrapOther)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def deriveNelWrap[A: TypeTag](encode: A => NonEmptyList[String], wrapOther: String => A): EnumWithOther[A] = {
    val values: Seq[A] = SumGeneric.EnumGeneric.deriveEnum.ignoreSingleCaseClass.values[A]
    EnumWithOther.make[A](values, encode, wrapOther)
  }

  inline def deriveSingleWrap[A: TypeTag](encode: A => String, wrapOther: String => A): EnumWithOther[A] = EnumWithOther.deriveNelWrap[A](a => NonEmptyList.one(encode(a)), wrapOther)

  inline def deriveWrap[A: {TypeTag, EnumEncoding as enc}](wrapOther: String => A): EnumWithOther[A] = EnumWithOther.deriveNelWrap[A](enc.encodeMany, wrapOther)

  inline def deriveNel[A: TypeTag](encode: A => NonEmptyList[String]): EnumWithOther[A] = {
    val (values: Seq[A], wrapOther: (String => A)) = SumGeneric.EnumGeneric.deriveEnum.ignoreSingleCaseClass.valuesAndWrap[A, String]
    EnumWithOther.make[A](values, encode, wrapOther)
  }

  inline def deriveSingle[A: TypeTag](encode: A => String): EnumWithOther[A] = EnumWithOther.deriveNel[A](a => NonEmptyList.one(encode(a)))
  inline def derive[A: TypeTag](encode: A => String): EnumWithOther[A] = EnumWithOther.deriveNel[A](a => NonEmptyList.one(encode(a)))

  inline def derive[A: {TypeTag, EnumEncoding as enc}]: EnumWithOther[A] = EnumWithOther.deriveNel[A](enc.encodeMany)

  inline def derived[A: {TypeTag, EnumEncoding as enc}]: EnumWithOther[A] = EnumWithOther.deriveNel[A](enc.encodeMany)

}
