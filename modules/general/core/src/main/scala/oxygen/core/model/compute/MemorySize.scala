package oxygen.core.model.compute

import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.common.*
import oxygen.core.syntax.number.*
import oxygen.core.typeclass.{StrictEnum, StringCodec}
import scala.annotation.implicitNotFound
import scala.util.matching.Regex

final case class MemorySize(
    value: Long,
    `type`: MemorySize.Type,
) {

  def totalBytes: Long = value * `type`.multiplier
  def toBytes: MemorySize = MemorySize(totalBytes, MemorySize.Type.Byte)
  def isRawBytes: Boolean = `type`.isRawBytes

  def showShort: String = s"$value${`type`.suffix}"
  def showLong: String = s"${value.toStringCommas} ${`type`.nameForAmount(value)}"
  def showVerbose: String =
    if this.isRawBytes then this.showLong
    else s"${this.showLong} (${this.toBytes.showLong})"

  override def toString: String = this.showShort

}
object MemorySize {

  ///////  ///////////////////////////////////////////////////////////////

  def bytes(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Byte)

  // 1024^x
  def kibibytes(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Kibibyte)
  def mebibytes(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Mebibyte)
  def gibibytes(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Gibibyte)
  def tebibytes(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Tebibyte)
  // 1024^x
  def kibs(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Kibibyte)
  def mibs(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Mebibyte)
  def gibs(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Gibibyte)
  def tibs(value: Long): MemorySize = MemorySize(value, MemorySize.Type.Tebibyte)

  // 1000^x
  def kilobytes(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Kilobyte)
  def megabytes(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Megabyte)
  def gigabytes(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Gigabyte)
  def terabytes(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Terabyte)
  // 1000^x
  def kbs(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Kilobyte)
  def mbs(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Megabyte)
  def gbs(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Gigabyte)
  def tbs(value: Long)(using MemorySize.AllowMultiplesOf1000): MemorySize = MemorySize(value, MemorySize.Type.Terabyte)

  ///////  ///////////////////////////////////////////////////////////////

  private val regex: Regex = "^([0-9]+)([A-Za-z]*)$".r
  def fromString(stringValue: String): Either[String, MemorySize] =
    for {
      (num, suffix) <- stringValue match
        case regex(num, suffix) => (num, suffix).asRight
        case _                  => "Malformed structure".asLeft
      value <- num.toLongOption.toRight(s"Unable to parse Long? ($num)")
      tpe <- MemorySize.Type.fromSuffix(suffix)
    } yield MemorySize(value, tpe)

  given StringCodec[MemorySize] = StringCodec.string.transformOrFail(fromString, _.showShort)

  enum Type(final val suffix: String, final val multiplier: Long) {

    final lazy val singular = this.productPrefix
    final lazy val plural = singular + "s"

    final def nameForAmount(value: Long): String =
      if value == 1 then singular else plural

    final lazy val isRawBytes: Boolean = this match
      case MemorySize.Type.Byte => true
      case _                    => false

    case Byte extends Type("", 1L)

    case Kibibyte extends Type("Ki", 1024L)
    case Mebibyte extends Type("Mi", 1024L * 1024L)
    case Gibibyte extends Type("Gi", 1024L * 1024L * 1024L)
    case Tebibyte extends Type("Ti", 1024L * 1024L * 1024L * 1024L)

    case Kilobyte extends Type("k", 1000L)
    case Megabyte extends Type("M", 1000L * 1000L)
    case Gigabyte extends Type("G", 1000L * 1000L * 1000L)
    case Terabyte extends Type("T", 1000L * 1000L * 1000L * 1000L)

    override final def toString: String = super.toString

  }
  object Type {

    val suffixStrictEnum: StrictEnum[MemorySize.Type] = StrictEnum.deriveSingle(_.suffix)
    val suffixSingularPluralStrictEnum: StrictEnum[MemorySize.Type] = StrictEnum.deriveNel { t => NonEmptyList.of(t.suffix, t.singular, t.plural) }

    given strictEnum: StrictEnum[MemorySize.Type] = suffixStrictEnum

    def fromSuffix(value: String): Either[String, MemorySize.Type] = suffixStrictEnum.decodeEitherWithHint(value)
    def fromSuffixOrName(value: String): Either[String, MemorySize.Type] = suffixSingularPluralStrictEnum.decodeEitherWithHint(value)

  }

  @implicitNotFound(
    "Memory sizes which are multiples of 1024 are vastly preferable to multiples of 1000.\nImport `MemorySize.AllowMultiplesOf1000.enabled.instance` if you meant to do this.\nIf not, look at the source of the function you are calling, it almost certainly has a `1024^x` sibling.",
  )
  final class AllowMultiplesOf1000 private ()
  object AllowMultiplesOf1000 {
    object enabled {
      given instance: AllowMultiplesOf1000 = new AllowMultiplesOf1000
    }
  }

}
