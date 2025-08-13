package oxygen.test

import java.time.*
import oxygen.predef.core.*
import oxygen.zio.instances.given
import zio.*

object RandomGen {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Enum
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def oneOf[S[_]: SeqOps, A](values: S[A]): UIO[A] = {
    val elems: Chunk[A] = values.into[Chunk]

    Random.nextIntBounded(elems.length).map(elems.apply)
  }

  def oneOfN[S[_]: SeqOps, A](values: S[A], n: Int): UIO[S[A]] = {
    val elems: Chunk[A] = values.into[Chunk]

    Random.nextIntBounded(elems.length).map(elems.apply).replicateZIO(n).map(_.into[S])
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Time
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val date2000: LocalDate = LocalDate.of(2000, 1, 1)
  private val date2050: LocalDate = LocalDate.of(2050, 1, 1)
  private val dateTime2000: LocalDateTime = date2000.atStartOfDay()
  private val dateTime2050: LocalDateTime = date2050.atStartOfDay()
  private val instant2000: Instant = dateTime2000.toInstant(ZoneOffset.UTC)
  private val instant2050: Instant = dateTime2050.toInstant(ZoneOffset.UTC)

  def localDateBetween(start: LocalDate = date2000, end: LocalDate = date2050): UIO[LocalDate] =
    Random.nextLongBetween(start.toEpochDay, end.toEpochDay).map(LocalDate.ofEpochDay)

  def localDateTimeBetween(start: LocalDateTime = dateTime2000, end: LocalDateTime = dateTime2050): UIO[LocalDateTime] =
    instantBetween(start.toInstant(ZoneOffset.UTC), end.toInstant(ZoneOffset.UTC)).map(LocalDateTime.ofInstant(_, ZoneOffset.UTC))

  def zonedDateTimeBetween(start: ZonedDateTime = dateTime2000.atZone(ZoneOffset.UTC), end: ZonedDateTime = dateTime2050.atZone(ZoneOffset.UTC)): UIO[ZonedDateTime] =
    instantBetween(start.toInstant, end.toInstant).map(ZonedDateTime.ofInstant(_, start.getZone))

  def offsetDateTimeBetween(start: OffsetDateTime = dateTime2000.atOffset(ZoneOffset.UTC), end: OffsetDateTime = dateTime2050.atOffset(ZoneOffset.UTC)): UIO[OffsetDateTime] =
    instantBetween(start.toInstant, end.toInstant).map(OffsetDateTime.ofInstant(_, start.getOffset))

  def instantBetween(start: Instant = instant2000, end: Instant = instant2050): UIO[Instant] =
    Random.nextLongBetween(start.toEpochMilli, end.toEpochMilli).map(Instant.ofEpochMilli)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Char/String
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def charBetween(min: Char, max: Char): UIO[Char] =
    Random.nextIntBetween(min.toInt, max.toInt + 1).map(_.toChar)

  def string(min: Char, max: Char, length: Int): UIO[String] =
    charBetween(min, max).replicateZIO(length).map(chars => new String(chars.toArray))

  def lowerCaseString(length: Int = 10): UIO[String] =
    string('a', 'z', length)

  def capitalizedString(length: Int = 10): UIO[String] =
    (charBetween('A', 'Z') <*> string('a', 'z', length - 1)).map { case (c, s) => c.toString + s }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Syntax
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object syntax {

    extension [A](self: Specified[A])
      def orGen(eff: => UIO[A]): UIO[A] = self match
        case Specified.WasSpecified(value) => ZIO.succeed(value)
        case Specified.WasNotSpecified     => eff

  }

}
