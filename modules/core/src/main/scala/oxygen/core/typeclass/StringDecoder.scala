package oxygen.core.typeclass

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime}
import oxygen.core.TypeTag
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*
import oxygen.core.syntax.string.*
import oxygen.core.syntax.throwable.*
import oxygen.core.syntax.traverse.*
import scala.util.Try

trait StringDecoder[A] { self =>

  val prevTypeInfo: TypeTag[?]
  val typeInfo: TypeTag[A]

  def decodeError(string: String): Either[StringDecoder.Error, A]
  final def decode(string: String): Either[String, A] = self.decodeError(string).leftMap(_.toString)
  final def decodeSimple(string: String): Either[String, A] = self.decodeError(string).leftMap(_.showSimple(true))

  final def cmapString(that: StringDecoder[String]): StringDecoder[A] =
    new StringDecoder[A] {
      override val prevTypeInfo: TypeTag[?] = that.typeInfo
      override val typeInfo: TypeTag[A] = self.typeInfo
      override def decodeError(string: String): Either[StringDecoder.Error, A] = that.decodeError(string).flatMap(self.decodeError)
    }

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
object StringDecoder {

  inline def apply[A](implicit ev: StringDecoder[A]): ev.type = ev

  implicit def fromCodec[A: StringCodec]: StringDecoder[A] = StringCodec[A].decoder

  // =====|  |=====

  final case class Error(
      prevTypeInfo: TypeTag[?],
      typeInfo: TypeTag[?],
      lastSuccessfulValue: Any,
      message: Option[String],
      hint: Option[Hint],
  ) {

    private def transformHintStr =
      if (prevTypeInfo == TypeTag[String]) s"Malformed {${typeInfo.prefixObject}}"
      else s"Failed to transform {${prevTypeInfo.prefixObject}}->{${typeInfo.prefixObject}}"

    def showSimple(includeHintWithMessage: Boolean): String =
      (message, hint) match
        case (Some(message), Some(hint)) if includeHintWithMessage => s"$message\n$hint"
        case (Some(message), _)                                    => message
        case (None, Some(hint))                                    => s"$transformHintStr: $hint"
        case (None, None)                                          => transformHintStr

    def showDetailed: String =
      (message, hint) match
        case (Some(message), Some(hint)) => s"$transformHintStr$message\n$hint"
        case (Some(message), None)       => s"$transformHintStr$message"
        case (None, Some(hint))          => s"$transformHintStr: $hint"
        case (None, None)                => transformHintStr

    override def toString: String = showDetailed

  }

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

  // =====|  |=====

  val string: StringDecoder[String] =
    new StringDecoder[String] {
      override val prevTypeInfo: TypeTag[?] = TypeTag[String]
      override val typeInfo: TypeTag[String] = TypeTag[String]
      override def decodeError(string: String): Either[StringDecoder.Error, String] = string.asRight
    }

  private val numsOneTwo = "(\\d{1,2})".r
  private val numsTwo = "(\\d{2})".r
  private val numsFour = "(\\d{4})".r
  private val optSpacing = "\\s*".r

  private val us2Year = s"$numsOneTwo/$numsOneTwo/$numsTwo".r
  private val us4Year = s"$numsOneTwo/$numsOneTwo/$numsFour".r
  private val other2Year = s"$numsOneTwo\\.$numsOneTwo\\.$numsTwo".r
  private val other4Year = s"$numsOneTwo\\.$numsOneTwo\\.$numsFour".r
  private val yearFirst = s"$numsFour-$numsOneTwo-$numsOneTwo".r

  private val hourAM = s"^$numsOneTwo$optSpacing(?:AM|am)$$".r
  private val hourPM = s"^$numsOneTwo$optSpacing(?:PM|pm)$$".r
  private val hourMinute = s"^$numsOneTwo:$numsTwo$$".r
  private val hourMinuteAM = s"^$numsOneTwo:$numsTwo$optSpacing(?:AM|am)$$".r
  private val hourMinutePM = s"^$numsOneTwo:$numsTwo$optSpacing(?:PM|pm)$$".r
  private val hourMinuteSecond = s"^$numsOneTwo:$numsTwo$numsTwo$$".r
  private val hourMinuteSecondAM = s"^$numsOneTwo:$numsTwo:$numsTwo$optSpacing(?:AM|am)$$".r
  private val hourMinuteSecondPM = s"^$numsOneTwo:$numsTwo:$numsTwo$optSpacing(?:PM|pm)$$".r

  private val spacesOrAt = "([^ ]+)[ ]+(?:@[ ]*)?(.+)".r

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

  private enum TimeUnit { case NS, MS, S, M, H, D, W }
  private object TimeUnit {

    def makeDuration(tu: TimeUnit): Long => Duration = tu match
      case TimeUnit.NS => Duration.ofNanos
      case TimeUnit.MS => Duration.ofMillis
      case TimeUnit.S  => Duration.ofSeconds
      case TimeUnit.M  => Duration.ofMinutes
      case TimeUnit.H  => Duration.ofHours
      case TimeUnit.D  => Duration.ofDays
      case TimeUnit.W  => d => Duration.ofDays(7 * d)

    private val lowerMapping: Map[String, TimeUnit] =
      Seq(
        NS -> Seq("ns", "nano", "nanos", "nanosecond", "nanoseconds"),
        MS -> Seq("ms"),
        S -> Seq("s", "sec", "secs", "second", "seconds"),
        M -> Seq("m", "min", "mins", "minute", "minutes"),
        H -> Seq("h", "hr", "hrs", "hour", "hours"),
        D -> Seq("d", "day", "days"),
        W -> Seq("w", "week", "weeks"),
      ).flatMap { case (tu, opts) => opts.map((_, tu)) }.toMap

    def unapply(string: String): Option[TimeUnit] = lowerMapping.get(string.toLowerCase.replaceAll("[ ]+", ""))

  }

  private val numberAndUnit = "^(\\d+)[ ]*(.+)$".r

  val duration: StringDecoder[Duration] =
    StringDecoder.string.mapOption {
      _.split("[ ]*\\+[ ]*").toSeq
        .traverse {
          case numberAndUnit(num, TimeUnit(tu)) => TimeUnit.makeDuration(tu)(num.toLong).some
          case _                                => None
        }
        .map(_.reduce(_.plus(_)))
    }

}
