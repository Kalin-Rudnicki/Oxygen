package oxygen.core.typeclass

import java.time.*
import java.util.{Base64, TimeZone, UUID}
import oxygen.core.TypeTag
import oxygen.core.syntax.either.*
import oxygen.core.syntax.string.*
import scala.util.Try

final case class StringCodec[A](
    encoder: StringEncoder[A],
    decoder: StringDecoder[A],
) { self =>

  // =====|  |=====

  def transformString(that: StringCodec[String]): StringCodec[A] =
    StringCodec(self.encoder.mapOutputString(that.encoder), self.decoder.mapInputString(that.decoder))

  /**
    * Transforms the value of this StringCodec in an infallible manner.
    */
  def transform[B: TypeTag](mapF: A => B, cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.map(mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: from provided either
    * hint-message: none
    */
  def transformOrFail[B: TypeTag](mapF: A => Either[String, B], cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapOrFail(mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: from provided either
    * hint-message: from provided hint
    */
  def transformOrFail[B: TypeTag](hint: StringDecoder.Hint, mapF: A => Either[String, B], cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapOrFail(hint, mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: none
    * hint-message: none
    */
  def transformOption[B: TypeTag](mapF: A => Option[B], cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapOption(mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: none
    * hint-message: from provided hint
    */
  def transformOption[B: TypeTag](hint: StringDecoder.Hint, mapF: A => Option[B], cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapOption(hint, mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: from caught throwable
    * hint-message: none
    */
  def transformCatchFail[B: TypeTag](mapF: A => B, cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapCatchFail(mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: from caught throwable
    * hint-message: from provided hint
    */
  def transformCatchFail[B: TypeTag](hint: StringDecoder.Hint, mapF: A => B, cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapCatchFail(hint, mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: none, caught throwable message is discarded
    * hint-message: none
    */
  def transformCatchOption[B: TypeTag](mapF: A => B, cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapCatchOption(mapF))

  /**
    * Attempts to transform the value of this StringCodec.
    * user-message: none, caught throwable message is discarded
    * hint-message: from provided hint
    */
  def transformCatchOption[B: TypeTag](hint: StringDecoder.Hint, mapF: A => B, cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapCatchOption(hint, mapF))

  // =====|  |=====

  /**
    * @see [[transform]]
    */
  def imap[B: TypeTag](mapF: A => B, cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.map(mapF))

  /**
    * @see [[transformOrFail]]
    */
  def iemap[B: TypeTag](mapF: A => Either[String, B], cmapF: B => A): StringCodec[B] =
    StringCodec(encoder.contramap(cmapF), decoder.mapOrFail(mapF))

}
object StringCodec {

  inline def apply[A](implicit ev: StringCodec[A]): ev.type = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: StringCodec[A])
    def @@(that: StringCodec[String]): StringCodec[A] =
      StringCodec(self.encoder >>> that.encoder, that.decoder >>> self.decoder)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given string: StringCodec[String] = StringCodec(StringEncoder.string, StringDecoder.string)

  given boolean: StringCodec[Boolean] = StringCodec.string.transformOption(_.toBooleanOption, _.toString)
  given uuid: StringCodec[UUID] = StringCodec.string.transformCatchOption(UUID.fromString, _.toString)

  // TODO (KR) : support hex
  given byte: StringCodec[Byte] = StringCodec.string.transformOption(parseInt(_.toByte, _.toByte), _.toString)
  given short: StringCodec[Short] = StringCodec.string.transformOption(parseInt(_.toShort, _.toShort), _.toString)
  given int: StringCodec[Int] = StringCodec.string.transformOption(parseInt(_.toInt, identity), _.toString)
  given long: StringCodec[Long] = StringCodec.string.transformOption(parseInt(_.toLong, identity), _.toString)
  given bigInt: StringCodec[BigInt] = StringCodec.string.transformOption(parseInt(BigInt(_), BigInt(_)), _.toString)

  given float: StringCodec[Float] = StringCodec.string.transformOption(_.toFloatOption, _.toString)
  given double: StringCodec[Double] = StringCodec.string.transformOption(_.toDoubleOption, _.toString)
  given bigDecimal: StringCodec[BigDecimal] = StringCodec.string.transformCatchOption(BigDecimal(_), _.toString)

  given period: StringCodec[Period] = StringCodec.string.transformCatchOption(Period.parse, _.toString)
  given instant: StringCodec[Instant] = StringCodec.string.transformCatchOption(Instant.parse, _.toString)
  given offsetDateTime: StringCodec[OffsetDateTime] = StringCodec.string.transformCatchOption(OffsetDateTime.parse(_), _.toString)
  given zonedDateTime: StringCodec[ZonedDateTime] = StringCodec.string.transformCatchOption(ZonedDateTime.parse(_), _.toString)
  given zoneId: StringCodec[ZoneId] = StringCodec.string.transformCatchOption(ZoneId.of, _.toString)
  given zoneOffset: StringCodec[ZoneOffset] = StringCodec.string.transformCatchOption(ZoneOffset.of, _.toString)
  given timeZone: StringCodec[TimeZone] = StringCodec.string.transformCatchOption(TimeZone.getTimeZone, _.getID)

  given localTime: StringCodec[LocalTime] =
    StringCodec(
      StringEncoder.usingToString,
      StringDecoder.string.mapCatchOption(LocalTime.parse(_)) <> StringDecoder.localTime,
    )
  given localDate: StringCodec[LocalDate] =
    StringCodec(
      StringEncoder.usingToString,
      StringDecoder.string.mapCatchOption(LocalDate.parse(_)) <> StringDecoder.localDate(LocalDate.now().getYear, 25),
    )
  given localDateTime: StringCodec[LocalDateTime] =
    StringCodec(
      StringEncoder.usingToString,
      StringDecoder.string.mapCatchOption(LocalDateTime.parse(_)) <> StringDecoder.localDateTime(LocalDate.now().getYear, 25),
    )
  given duration: StringCodec[Duration] =
    StringCodec(
      StringEncoder.usingToString,
      StringDecoder.duration <> StringDecoder.string.mapCatchOption(Duration.parse),
    )

  given `class`: StringCodec[Class[?]] =
    StringCodec.string.transform(str => Try { Class.forName(str) }.getOrElse { classOf[Any] }, _.getName)

  given strictEnum: [A: StrictEnum as e] => StringCodec[A] =
    StringCodec.string.transformOption(StringDecoder.Hint.AllowedValues(e.encodedValues), e.decodeOption, e.encode)(using e.typeTag)
  given enumWithOther: [A: EnumWithOther as e] => StringCodec[A] =
    StringCodec.string.transform(e.decode, e.encode)(using e.typeTag)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Manual
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val base64: StringCodec[String] = base64Codec("", Base64.getEncoder, Base64.getDecoder)
  val base64Url: StringCodec[String] = base64Codec(" (url)", Base64.getUrlEncoder, Base64.getUrlDecoder)
  val base64Mime: StringCodec[String] = base64Codec(" (mime)", Base64.getMimeEncoder, Base64.getMimeDecoder)

  val base64NoPadding: StringCodec[String] = base64Codec("", Base64.getEncoder.withoutPadding, Base64.getDecoder)
  val base64UrlNoPadding: StringCodec[String] = base64Codec(" (url)", Base64.getUrlEncoder.withoutPadding, Base64.getUrlDecoder)
  val base64MimeNoPadding: StringCodec[String] = base64Codec(" (mime)", Base64.getMimeEncoder.withoutPadding, Base64.getMimeDecoder)

  def wrappedString(prefix: String, suffix: String): StringCodec[String] =
    StringCodec.string.transformOrFail(
      { string =>
        if (string.startsWith(prefix)) {
          val tmp: String = string.stripPrefix(prefix)
          if (tmp.endsWith(suffix))
            tmp.stripSuffix(suffix).asRight
          else
            s"Missing suffix ${suffix.unesc}".asLeft
        } else
          s"Missing prefix ${prefix.unesc}".asLeft
      },
      string => s"$prefix$string$suffix",
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val intBase10Regex = "^(-?\\d+)$".r
  private val intBase16Regex = "^0x(\\d+)$".r
  private def parseInt[A](base10: String => A, base16: Int => A)(string: String): Option[A] =
    string match
      case intBase10Regex(string) => Try { base10(string) }.toOption
      case intBase16Regex(string) => Try { base16(Integer.parseInt(string, 16)) }.toOption
      case _                      => None

  private def base64Codec(suffix: String, encoder: Base64.Encoder, decoder: Base64.Decoder): StringCodec[String] =
    StringCodec.string.transformOrFail(
      base64String => Try { new String(decoder.decode(base64String)) }.toOption.toRight(s"invalid base-64$suffix"),
      normalString => encoder.encodeToString(normalString.getBytes),
    )

}

// TODO (KR) : low-priority fromEncoderAndDecoder
