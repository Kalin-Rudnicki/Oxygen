package oxygen.test

import oxygen.predef.core.*
import zio.*

object Generators {

  def oneOf[S[_]: SeqOps, A](values: S[A]): UIO[A] = {
    val elems: Contiguous[A] = values.into[Contiguous]

    Random.nextIntBounded(elems.length).map(elems.at)
  }

  def charBetween(min: Char, max: Char): UIO[Char] =
    Random.nextIntBetween(min.toInt, max.toInt + 1).map(_.toChar)

  def string(min: Char, max: Char, length: Int): UIO[String] =
    charBetween(min, max).replicateZIO(length).map(chars => new String(chars.toArray))

  def lowerCaseString(length: Int = 10): UIO[String] =
    string('a', 'z', length)

  def capitalizedString(length: Int = 10): UIO[String] =
    (charBetween('A', 'Z') <*> string('a', 'z', length - 1)).map { case (c, s) => c.toString + s }

  object syntax {

    extension [A](self: Specified[A])
      def orGen(eff: => UIO[A]): UIO[A] = self match
        case Specified.WasSpecified(value) => ZIO.succeed(value)
        case Specified.WasNotSpecified     => eff

  }

}
