package oxygen.slyce.core.generic

import oxygen.core.InfiniteSet
import oxygen.predef.core.*
import scala.annotation.tailrec

private[generic] sealed trait ParsedRegex {

  final def exactly(n: Int): ParsedRegex = ParsedRegex.WithQuant(this, ParsedRegex.Quant.Exactly(n))
  final def atLeast(min: Int): ParsedRegex = ParsedRegex.WithQuant(this, ParsedRegex.Quant.AtLeast(min))
  final def atMost(max: Int): ParsedRegex = this.between(0, max)
  final def between(min: Int, max: Int): ParsedRegex = ParsedRegex.WithQuant(this, ParsedRegex.Quant.Between(min, max))
  final def ? : ParsedRegex = this.atMost(1)
  final def * : ParsedRegex = this.atLeast(0)
  final def + : ParsedRegex = this.atLeast(1)

  final def |(that: ParsedRegex): ParsedRegex = (this.simplify, that.simplify) match
    case (self: ParsedRegex.CharClass, that: ParsedRegex.CharClass) => self | that
    case (self, that)                                               => ParsedRegex.Group(self.toGroup.sequences ++ that.toGroup.sequences)

  final def >>>(that: ParsedRegex): ParsedRegex = ParsedRegex.Sequence.ofElems(this, that)

}
private[generic] object ParsedRegex {

  final case class CharClass(chars: InfiniteSet[Char]) extends ParsedRegex {
    def |(that: CharClass): CharClass = CharClass(this.chars | that.chars)
  }
  object CharClass {
    def inclusive(chars: Set[Char]): CharClass = CharClass(InfiniteSet.Inclusive(chars))
    def inclusive(chars: Char*): CharClass = CharClass(InfiniteSet.Inclusive(chars*))
    def exclusive(chars: Set[Char]): CharClass = CharClass(InfiniteSet.Exclusive(chars))
    def exclusive(chars: Char*): CharClass = CharClass(InfiniteSet.Exclusive(chars*))
    def anything: CharClass = CharClass(InfiniteSet.full)
  }

  final case class WithQuant(inner: ParsedRegex, quant: Quant) extends ParsedRegex

  sealed trait Group extends ParsedRegex {
    def sequences: NonEmptyList[Sequence]
  }
  object Group {

    def apply(sequences: NonEmptyList[Sequence]): Group = sequences match
      case NonEmptyList(sequence, Nil) => sequence
      case _                           => NelGroup(sequences)

    def of(seq0: Sequence, seqN: Sequence*): Group = Group(NonEmptyList(seq0, seqN.toList))

  }

  final case class NelGroup private[ParsedRegex] (sequences: NonEmptyList[Sequence]) extends Group

  final case class Sequence(elems: List[ParsedRegex]) extends Group {
    override def sequences: NonEmptyList[Sequence] = NonEmptyList.one(this)
  }
  object Sequence {
    def ofElems(elems: ParsedRegex*): Sequence = Sequence(elems.toList.flatMap(_.flattenElems))
    def ofString(str: String): Sequence = Sequence(str.toList.map(CharClass.inclusive(_)))
    given Conversion[String, Sequence] = Sequence.ofString(_)
  }

  enum Quant {
    case Exactly(n: Int)
    case AtLeast(min: Int)
    case Between(min: Int, max: Int)
  }
  object Quant {
    def ? : Quant = Quant.Between(0, 1)
    def * : Quant = Quant.AtLeast(0)
    def + : Quant = Quant.AtLeast(1)
  }

  extension (self: ParsedRegex) {

    @tailrec
    def simplify: ParsedRegex = self match
      case NelGroup(NonEmptyList(head, Nil)) => head.simplify
      case Sequence(head :: Nil)             => head.simplify
      case _                                 => self

    def toGroup: Group = self match
      case group: Group => group
      case _            => Sequence.ofElems(self)

    def toSequence: Sequence = self.simplify match
      case seq: Sequence => seq
      case _             => Sequence.ofElems(self)

    def flattenElems: List[ParsedRegex] = self.simplify match
      case Sequence(elems) => elems
      case _               => self :: Nil

  }

  given Conversion[Char, ParsedRegex] = CharClass.inclusive(_)
  given Conversion[String, ParsedRegex] = Sequence.ofString(_)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Parsing
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private trait RegexParser[+A] {

    def parse(chars: IArray[Char], index: Int): RegexParser.ParseResult[A]

  }
  private object RegexParser {

    sealed trait ParseResult[+A]
    object ParseResult {
      final case class Success[+A](value: A, remainingIndex: Int) extends ParseResult[A]
      final case class Failure(error: String) extends ParseResult[Nothing]
      final case class Unknown(consumed: Boolean, remainingIndex: Int) extends ParseResult[Nothing]
    }

  }

  def parse(regex: String): Either[String, ParsedRegex] =
    ??? // FIX-PRE-MERGE (KR) :

}
