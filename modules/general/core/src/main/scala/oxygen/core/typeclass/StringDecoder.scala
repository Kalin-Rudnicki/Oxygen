package oxygen.core.typeclass

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime}
import oxygen.core.TypeTag
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*
import oxygen.core.syntax.string.*
import oxygen.core.syntax.throwable.*
import oxygen.core.syntax.traverse.*
import oxygen.core.typeclass.StringDecoderTimeUtils.*
import scala.util.Try
import scala.util.matching.Regex

/**
  * Represents the ability to decode a [[String]] into an [[A]].
  * Once an [[A]] is decoded, you can then use standard helpers like [[map]]/[[mapOrFail]] to convert the [[A]] into some other `B`.
  * In the event you are not able to successfully decode an [[A]], [[StringDecoder.Error]] keeps track of:
  * - [[StringDecoder.Error.prevTypeInfo]]        : type of the last successfully decoded value
  * - [[StringDecoder.Error.typeInfo]]            : type you are trying to decode
  * - [[StringDecoder.Error.lastSuccessfulValue]] : last successfully decoded value
  * - [[StringDecoder.Error.message]]             : optional message of the failed decode
  * - [[StringDecoder.Error.hint]]                : optional message of the failed decode
  */
trait StringDecoder[A] { self =>

  val prevTypeInfo: TypeTag[?]
  val typeInfo: TypeTag[A]

  def decodeError(string: String): Either[StringDecoder.Error, A]

  final def decodeSimple(string: String): Either[String, A] = self.decodeError(string).leftMap(_.showSimple(true))
  final def decodeDetailed(string: String): Either[String, A] = self.decodeError(string).leftMap(_.showDetailed)
  final def decode(string: String): Either[String, A] = self.decodeDetailed(string)

  final def mapInputString(that: StringDecoder[String]): StringDecoder[A] = StringDecoder.MapInputString(this, that)

  /**
    * Maps the value of this StringDecoder in an infallible manner.
    */
  final def map[B: TypeTag](f: A => B): StringDecoder[B] =
    new StringDecoder[B] {
      override val prevTypeInfo: TypeTag[?] = self.typeInfo
      override val typeInfo: TypeTag[B] = TypeTag[B]
      override def decodeError(string: String): Either[StringDecoder.Error, B] =
        self.decodeError(string).map(f)
    }

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: from provided either
    * hint-message: none
    */
  final def mapOrFail[B: TypeTag](f: A => Either[String, B]): StringDecoder[B] =
    new StringDecoder[B] {
      override val prevTypeInfo: TypeTag[?] = self.typeInfo
      override val typeInfo: TypeTag[B] = TypeTag[B]
      override def decodeError(string: String): Either[StringDecoder.Error, B] =
        self.decodeError(string).flatMap(v => f(v).leftMap(e => StringDecoder.Error(prevTypeInfo, typeInfo, v, e.some, None)))
    }

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: from provided either
    * hint-message: from provided hint
    */
  final def mapOrFail[B: TypeTag](hint: StringDecoder.Hint, f: A => Either[String, B]): StringDecoder[B] =
    new StringDecoder[B] {
      override val prevTypeInfo: TypeTag[?] = self.typeInfo
      override val typeInfo: TypeTag[B] = TypeTag[B]
      override def decodeError(string: String): Either[StringDecoder.Error, B] =
        self.decodeError(string).flatMap(v => f(v).leftMap(e => StringDecoder.Error(prevTypeInfo, typeInfo, v, e.some, hint.some)))
    }

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: none
    * hint-message: none
    */
  final def mapOption[B: TypeTag](f: A => Option[B]): StringDecoder[B] =
    new StringDecoder[B] {
      override val prevTypeInfo: TypeTag[?] = self.typeInfo
      override val typeInfo: TypeTag[B] = TypeTag[B]
      override def decodeError(string: String): Either[StringDecoder.Error, B] =
        self.decodeError(string).flatMap(v => f(v).toRight(StringDecoder.Error(prevTypeInfo, typeInfo, v, None, None)))
    }

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: none
    * hint-message: from provided hint
    */
  final def mapOption[B: TypeTag](hint: StringDecoder.Hint, f: A => Option[B]): StringDecoder[B] =
    new StringDecoder[B] {
      override val prevTypeInfo: TypeTag[?] = self.typeInfo
      override val typeInfo: TypeTag[B] = TypeTag[B]
      override def decodeError(string: String): Either[StringDecoder.Error, B] =
        self.decodeError(string).flatMap(v => f(v).toRight(StringDecoder.Error(prevTypeInfo, typeInfo, v, None, hint.some)))
    }

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: from caught throwable
    * hint-message: none
    */
  final def mapCatchFail[B: TypeTag](f: A => B): StringDecoder[B] =
    self.mapOrFail(s => Try { f(s) }.toEither.leftMap(_.safeGetMessage))

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: from caught throwable
    * hint-message: from provided hint
    */
  final def mapCatchFail[B: TypeTag](hint: StringDecoder.Hint, f: A => B): StringDecoder[B] =
    self.mapOrFail(hint, s => Try { f(s) }.toEither.leftMap(_.safeGetMessage))

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: none, caught throwable message is discarded
    * hint-message: none
    */
  final def mapCatchOption[B: TypeTag](f: A => B): StringDecoder[B] =
    self.mapOption(s => Try { f(s) }.toOption)

  /**
    * Attempts to map the value of this StringDecoder.
    * user-message: none, caught throwable message is discarded
    * hint-message: from provided hint
    */
  final def mapCatchOption[B: TypeTag](hint: StringDecoder.Hint, f: A => B): StringDecoder[B] =
    self.mapOption(hint, s => Try { f(s) }.toOption)

  final def <>[B >: A](that: StringDecoder[B]): StringDecoder[B] =
    new StringDecoder[B] {
      override val prevTypeInfo: TypeTag[?] = that.prevTypeInfo
      override val typeInfo: TypeTag[B] = that.typeInfo
      override def decodeError(string: String): Either[StringDecoder.Error, B] =
        self.decodeError(string).orElse(that.decodeError(string))
    }

  final def unapply(string: String): Option[A] = self.decodeError(string).toOption

}
object StringDecoder extends StringDecoderLowPriority.LowPriority1 {

  inline def apply[A](implicit ev: StringDecoder[A]): ev.type = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: StringDecoder[String])
    def >>>[A](that: StringDecoder[A]): StringDecoder[A] =
      StringDecoder.MapInputString(that, self)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Identity extends StringDecoder[String] {
    override val prevTypeInfo: TypeTag[?] = TypeTag[String]
    override val typeInfo: TypeTag[String] = TypeTag[String]
    override def decodeError(string: String): Either[StringDecoder.Error, String] = string.asRight
  }

  case object Trimmed extends StringDecoder[String] {
    override val prevTypeInfo: TypeTag[?] = TypeTag[String]
    override val typeInfo: TypeTag[String] = TypeTag[String]
    override def decodeError(string: String): Either[StringDecoder.Error, String] = string.trim.asRight
  }

  final case class MapInputString[A](base: StringDecoder[A], in: StringDecoder[String]) extends StringDecoder[A] {
    override val prevTypeInfo: TypeTag[?] = in.typeInfo
    override val typeInfo: TypeTag[A] = base.typeInfo
    override def decodeError(string: String): Either[StringDecoder.Error, A] = in.decodeError(string).flatMap(base.decodeError)
  }

  val string: StringDecoder[String] = Identity

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Time
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def localDate(_currentYear: => Int, futureTolerance: => Int): StringDecoder[LocalDate] = {
    def guessYear(y: Int): Int = {
      val currentYear = _currentYear
      val currentCentury = currentYear / 100
      val currentYearInCentury = currentYear % 100
      val futureYearInCentury = currentYearInCentury + futureTolerance
      val centuryGuess =
        if (futureYearInCentury >= 100)
          if (y >= currentYearInCentury) currentCentury
          else if (y <= futureYearInCentury % 100) currentCentury + 1
          else currentCentury
        else if (y <= futureYearInCentury) currentCentury
        else currentCentury - 1

      centuryGuess * 100 + y
    }

    def attemptDate(date: => LocalDate): Option[LocalDate] =
      Try { date }.toOption

    StringDecoder.string.mapOption {
      case us2Year(month, day, year)    => attemptDate(LocalDate.of(guessYear(year.toInt), month.toInt, day.toInt))
      case us4Year(month, day, year)    => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case other2Year(day, month, year) => attemptDate(LocalDate.of(guessYear(year.toInt), month.toInt, day.toInt))
      case other4Year(day, month, year) => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case yearFirst(year, month, day)  => attemptDate(LocalDate.of(year.toInt, month.toInt, day.toInt))
      case _                            => None
    }
  }

  val localTime: StringDecoder[LocalTime] = {
    def attemptTime(time: => LocalTime): Option[LocalTime] =
      Try(time).toOption

    def amTime(hour: Int): Int =
      if (hour < 0) throw new IllegalArgumentException("am-time < 0")
      else if (hour > 12) throw new IllegalArgumentException("am-time > 12")
      else if (hour == 12) 0
      else hour

    def pmTime(hour: Int): Int =
      if (hour < 0) throw new IllegalArgumentException("pm-time < 0")
      else if (hour > 12) throw new IllegalArgumentException("pm-time > 12")
      else if (hour == 12) 12
      else hour + 12

    StringDecoder.string.mapOption {
      case hourAM(hour)                             => attemptTime(LocalTime.of(amTime(hour.toInt), 0))
      case hourPM(hour)                             => attemptTime(LocalTime.of(pmTime(hour.toInt), 0))
      case hourMinute(hour, minute)                 => attemptTime(LocalTime.of(hour.toInt, minute.toInt))
      case hourMinuteAM(hour, minute)               => attemptTime(LocalTime.of(amTime(hour.toInt), minute.toInt))
      case hourMinutePM(hour, minute)               => attemptTime(LocalTime.of(pmTime(hour.toInt), minute.toInt))
      case hourMinuteSecond(hour, minute, second)   => attemptTime(LocalTime.of(hour.toInt, minute.toInt, second.toInt))
      case hourMinuteSecondAM(hour, minute, second) => attemptTime(LocalTime.of(amTime(hour.toInt), minute.toInt, second.toInt))
      case hourMinuteSecondPM(hour, minute, second) => attemptTime(LocalTime.of(pmTime(hour.toInt), minute.toInt, second.toInt))
      case _                                        => None
    }
  }

  def localDateTime(_currentYear: => Int, futureTolerance: => Int): StringDecoder[LocalDateTime] = {
    val localDate = StringDecoder.localDate(_currentYear, futureTolerance)
    StringDecoder.string.mapOption {
      case spacesOrAt(localDate(ld), localTime(lt)) => LocalDateTime.of(ld, lt).some
      case localDate(ld)                            => ld.atStartOfDay.some
      case _                                        => None
    }
  }

  val duration: StringDecoder[Duration] =
    StringDecoder.string.mapOption {
      _.split("[ ]*\\+[ ]*").toSeq
        .traverse {
          case numberAndUnit(num, DecoderTimeUnit(tu)) => DecoderTimeUnit.makeDuration(tu)(num.toLong).some
          case _                                       => None
        }
        .map(_.reduce(_.plus(_)))
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Error / Hint
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Error(
      prevTypeInfo: TypeTag[?],
      typeInfo: TypeTag[?],
      lastSuccessfulValue: Any,
      message: Option[String],
      hint: Option[Hint],
  ) {

    private lazy val cameFromString: Boolean = prevTypeInfo == TypeTag[String]

    private def transformHintStr: String =
      if (cameFromString) s"Malformed {${typeInfo.prefixObject}}"
      else s"Failed to transform {${prevTypeInfo.prefixObject}}->{${typeInfo.prefixObject}}"

    def showSimple(includeHintWithMessage: Boolean): String =
      (message, hint) match
        case (Some(message), Some(hint)) if includeHintWithMessage => s"$message ~ $hint"
        case (Some(message), _)                                    => message
        case (None, Some(hint))                                    => s"$transformHintStr ~ $hint"
        case (None, None)                                          => transformHintStr

    def showDetailed: String =
      (message, hint) match
        case (Some(message), Some(hint)) => s"$transformHintStr ~ $message ~ $hint"
        case (Some(message), None)       => s"$transformHintStr ~ $message"
        case (None, Some(hint))          => s"$transformHintStr ~ $hint"
        case (None, None)                => transformHintStr

    override def toString: String = showDetailed

  }

  // TODO (KR) : move this logic exclusively to StringCodec or PlainTextSchema
  sealed trait Hint {

    override final def toString: String = this match
      case Hint.AllowedValues(values)   => s"Allowed values: ${values.map(_.unesc).mkString(", ")}"
      case Hint.AllowedFormats(formats) => s"Allowed formats: ${formats.map(_.unesc).mkString(", ")}"

  }
  object Hint {

    final case class AllowedValues(values: Seq[String]) extends Hint
    final case class AllowedFormats(formats: Seq[String]) extends Hint

    def allowedValues(value0: String, valueN: String*): Hint.AllowedValues = Hint.AllowedValues(value0 +: valueN)
    def allowedFormats(format0: String, formatN: String*): Hint.AllowedFormats = Hint.AllowedFormats(format0 +: formatN)

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Instances (Low Priority)
//////////////////////////////////////////////////////////////////////////////////////////////////////

object StringDecoderLowPriority {

  trait LowPriority1 {

    given fromCodec: [A: StringCodec as codec] => StringDecoder[A] = codec.decoder

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Helpers
//////////////////////////////////////////////////////////////////////////////////////////////////////

private[typeclass] object StringDecoderTimeUtils {

  val spacesOrAt: Regex = "([^ ]+)[ ]+(?:@[ ]*)?(.+)".r

  val numsOneTwo: Regex = "(\\d{1,2})".r
  val numsTwo: Regex = "(\\d{2})".r
  val numsFour: Regex = "(\\d{4})".r
  val optSpacing: Regex = "\\s*".r

  val us2Year: Regex = s"$numsOneTwo/$numsOneTwo/$numsTwo".r
  val us4Year: Regex = s"$numsOneTwo/$numsOneTwo/$numsFour".r
  val other2Year: Regex = s"$numsOneTwo\\.$numsOneTwo\\.$numsTwo".r
  val other4Year: Regex = s"$numsOneTwo\\.$numsOneTwo\\.$numsFour".r
  val yearFirst: Regex = s"$numsFour-$numsOneTwo-$numsOneTwo".r

  val hourAM: Regex = s"^$numsOneTwo$optSpacing(?:AM|am)$$".r
  val hourPM: Regex = s"^$numsOneTwo$optSpacing(?:PM|pm)$$".r
  val hourMinute: Regex = s"^$numsOneTwo:$numsTwo$$".r
  val hourMinuteAM: Regex = s"^$numsOneTwo:$numsTwo$optSpacing(?:AM|am)$$".r
  val hourMinutePM: Regex = s"^$numsOneTwo:$numsTwo$optSpacing(?:PM|pm)$$".r
  val hourMinuteSecond: Regex = s"^$numsOneTwo:$numsTwo$numsTwo$$".r
  val hourMinuteSecondAM: Regex = s"^$numsOneTwo:$numsTwo:$numsTwo$optSpacing(?:AM|am)$$".r
  val hourMinuteSecondPM: Regex = s"^$numsOneTwo:$numsTwo:$numsTwo$optSpacing(?:PM|pm)$$".r

  val numberAndUnit: Regex = "^(\\d+)[ ]*(.+)$".r

  enum DecoderTimeUnit { case NS, MS, S, M, H, D, W }
  object DecoderTimeUnit {

    def makeDuration(tu: DecoderTimeUnit): Long => Duration = tu match
      case DecoderTimeUnit.NS => Duration.ofNanos
      case DecoderTimeUnit.MS => Duration.ofMillis
      case DecoderTimeUnit.S  => Duration.ofSeconds
      case DecoderTimeUnit.M  => Duration.ofMinutes
      case DecoderTimeUnit.H  => Duration.ofHours
      case DecoderTimeUnit.D  => Duration.ofDays
      case DecoderTimeUnit.W  => d => Duration.ofDays(7 * d)

    private val lowerMapping: Map[String, DecoderTimeUnit] =
      Seq(
        NS -> Seq("ns", "nano", "nanos", "nanosecond", "nanoseconds"),
        MS -> Seq("ms"),
        S -> Seq("s", "sec", "secs", "second", "seconds"),
        M -> Seq("m", "min", "mins", "minute", "minutes"),
        H -> Seq("h", "hr", "hrs", "hour", "hours"),
        D -> Seq("d", "day", "days"),
        W -> Seq("w", "week", "weeks"),
      ).flatMap { case (tu, opts) => opts.map((_, tu)) }.toMap

    def unapply(string: String): Option[DecoderTimeUnit] = lowerMapping.get(string.toLowerCase.replaceAll("[ ]+", ""))

  }

}
