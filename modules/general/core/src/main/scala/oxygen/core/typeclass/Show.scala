package oxygen.core.typeclass

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.*
import oxygen.core.syntax.string.*
import oxygen.core.syntax.throwable.*
import scala.collection.mutable

trait Show[A] {

  def show(value: A): String

  def writeTo(builder: mutable.StringBuilder, value: A): Unit = builder.append(show(value))

}
object Show extends ShowLowPriority.LowPriority1 {

  inline def apply[A: Show as ev]: Show[A] = ev

  trait PreferBuffer[A] extends Show[A] {

    override def show(value: A): String = {
      val builder = mutable.StringBuilder()
      writeTo(builder, value)
      builder.result()
    }

    override def writeTo(builder: mutable.StringBuilder, value: A): Unit

  }

  final class ToString[A] extends Show[A] {
    override def show(value: A): String = value.toString
  }
  object ToString {
    inline def derived[A]: Show.ToString[A] = new ToString[A]
  }

  def usingToString[A]: Show[A] = new ToString[A]

  def shown[A](f: A => String): Show[A] = f(_)

  given string: Show[String] = _.unesc
  given boolean: Show[Boolean] = usingToString
  given uuid: Show[UUID] = usingToString
  given byte: Show[Byte] = usingToString
  given short: Show[Short] = usingToString
  given int: Show[Int] = usingToString
  given long: Show[Long] = usingToString
  given bigInt: Show[BigInt] = usingToString
  given float: Show[Float] = usingToString
  given double: Show[Double] = usingToString
  given bigDecimal: Show[BigDecimal] = usingToString

  given localTime: Show[LocalTime] = usingToString
  given localDate: Show[LocalDate] = usingToString
  given localDateTime: Show[LocalDateTime] = usingToString
  given zonedDateTime: Show[ZonedDateTime] = usingToString
  given offsetDateTime: Show[OffsetDateTime] = usingToString
  given offsetTime: Show[OffsetTime] = usingToString
  given instant: Show[Instant] = usingToString
  given duration: Show[Duration] = usingToString
  given period: Show[Period] = usingToString
  given zoneId: Show[ZoneId] = usingToString
  given zoneOffset: Show[ZoneOffset] = usingToString
  given timeZone: Show[TimeZone] = usingToString
  given monthDay: Show[MonthDay] = usingToString
  given year: Show[Year] = usingToString
  given yearMonth: Show[YearMonth] = usingToString
  given month: Show[Month] = usingToString

  given throwable: Show[Throwable] = _.safeGetMessage

  def showOption[A: Show as showA](
      somePrefix: String,
      someSuffix: String,
      none: String,
  ): Show[Option[A]] =
    new PreferBuffer[Option[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: Option[A]): Unit =
        value match {
          case Some(value) =>
            builder.append(somePrefix)
            showA.writeTo(builder, value)
            builder.append(someSuffix)
          case None =>
            builder.append(none)
        }
    }

  given option: [A: Show as showA] => Show[Option[A]] =
    showOption("", "", "<none>")

  def showSpecified[A: Show as showA](
      specifiedPrefix: String,
      specifiedSuffix: String,
      unspecified: String,
  ): Show[Specified[A]] =
    new PreferBuffer[Specified[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: Specified[A]): Unit =
        value match {
          case Specified.WasSpecified(value) =>
            builder.append(specifiedPrefix)
            showA.writeTo(builder, value)
            builder.append(specifiedSuffix)
          case Specified.WasNotSpecified =>
            builder.append(unspecified)
        }
    }

  given specified: [A: Show as showA] => Show[Specified[A]] =
    showSpecified("", "", "<unspecified>")

  def showSeq[S[_]: SeqRead as seqOps, A: Show as showA](
      prefix: String,
      join: String,
      suffix: String,
  ): Show[S[A]] =
    new PreferBuffer[S[A]] {
      override def writeTo(builder: mutable.StringBuilder, value: S[A]): Unit = {
        val iter = seqOps.newIterator(value)
        builder.append(prefix)
        iter.nextOption() match {
          case Some(value) =>
            showA.writeTo(builder, value)
          case None =>
        }
        while (iter.hasNext) {
          builder.append(join)
          showA.writeTo(builder, iter.next())
        }
        builder.append(suffix)
      }
    }

  final case class Shown(value: String)
  object Shown {

    def show[A: Show as showA](value: A): Shown = Shown(showA.show(value))

    given showableToShown: [A: Show as s] => Conversion[A, Shown] = a => Shown(s.show(a))

  }

}

object ShowLowPriority {

  trait LowPriority1 {

    given seq: [S[_]: SeqRead as seqOps, A: Show as showA] => Show[S[A]] =
      Show.showSeq[S, A]("[", ", ", "]")

    given map: [K: Show as showKey, V: Show as showValue] => Show[Map[K, V]] =
      _.iterator.map { (k, v) => s" ${showKey.show(k)} : ${showValue.show(v)} " }.mkString("{", ",", "}")

  }

}
