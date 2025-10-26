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

  private[ParsedRegex] final def showInternal: String =
    this match {
      case ParsedRegex.CharClass(chars: InfiniteSet.Inclusive[Char]) if chars.explicit.size == 1 => chars.explicit.head.unesc("")
      case ParsedRegex.CharClass(chars: InfiniteSet.Inclusive[Char])                             => s"[${chars.explicit.toSeq.sorted.map(_.unesc("")).mkString}]"
      case ParsedRegex.CharClass(chars: InfiniteSet.Exclusive[Char])                             => s"[^${chars.explicit.toSeq.sorted.map(_.unesc("")).mkString}]"
      case ParsedRegex.WithQuant(inner: ParsedRegex.Group, quant)                                => s"(${inner.showInternal})${quant.show}"
      case ParsedRegex.WithQuant(inner, quant)                                                   => s"${inner.showInternal}${quant.show}"
      case ParsedRegex.Sequence(elems)                                                           => elems.map(_.showInternal).mkString
      case ParsedRegex.NelGroup(seqs)                                                            => seqs.map(_.showInternal).mkString("|")

    }

  final def show: String =
    this.toSequences.map(_.showInternal).mkString("|")

  override final def toString: String = show

}
private[generic] object ParsedRegex {

  final case class CharClass(chars: InfiniteSet[Char]) extends ParsedRegex {
    def unary_! : CharClass = CharClass(chars.invert)
    def |(that: CharClass): CharClass = CharClass(this.chars | that.chars)
  }
  object CharClass {

    def inclusive(chars: Set[Char]): CharClass = CharClass(InfiniteSet.Inclusive(chars))
    def inclusive(chars: Char*): CharClass = CharClass(InfiniteSet.Inclusive(chars*))
    def inclusiveRange(a: Char, b: Char): CharClass = CharClass.inclusive((a min b).to(a max b).toSet)

    def exclusive(chars: Set[Char]): CharClass = CharClass(InfiniteSet.Exclusive(chars))
    def exclusive(chars: Char*): CharClass = CharClass(InfiniteSet.Exclusive(chars*))
    def exclusiveRange(a: Char, b: Char): CharClass = CharClass.exclusive((a min b).to(a max b).toSet)

    def anything: CharClass = CharClass(InfiniteSet.full)

  }

  final case class WithQuant(inner: ParsedRegex, quant: Quant) extends ParsedRegex

  sealed trait Group extends ParsedRegex {
    def sequences: NonEmptyList[Sequence]
  }
  object Group {

    def apply(sequences: NonEmptyList[Sequence]): Group = NelGroup(sequences).toGroup

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

    final def show: String = this match
      case Quant.Exactly(n)        => s"{$n}"
      case Quant.AtLeast(min)      => s"{$min,}"
      case Quant.Between(min, max) => s"{$min,$max}"

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

    def toSequences: NonEmptyList[Sequence] =
      self.simplify match {
        case NelGroup(seqs) => seqs.flatMap(_.toSequences)
        case seq: Sequence  => NonEmptyList.one(seq)
        case reg            => NonEmptyList.one(Sequence.ofElems(reg))
      }

    def toGroup: Group = {
      val tmp: Ior[NonEmptyList[CharClass], NonEmptyList[ParsedRegex]] =
        self.toSequences.partitionMap {
          _.simplify match {
            case cc: CharClass => cc.asLeft
            case reg           => reg.asRight
          }
        }

      tmp match {
        case Ior.Both(left, right)               => NelGroup(NonEmptyList(Sequence.ofElems(left.reduceLeft { _ | _ }), right.map(_.toSequence).toList))
        case Ior.Left(left)                      => Sequence.ofElems(left.reduceLeft { _ | _ })
        case Ior.Right(NonEmptyList(right, Nil)) => right.toSequence
        case Ior.Right(seqs)                     => NelGroup(seqs.map(_.toSequence))
      }
    }

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

    final def ? : RegexParser[Option[A]] = RegexParser.Optional(this)
    final def * : RegexParser[List[A]] = RegexParser.Many(this)
    final def + : RegexParser[NonEmptyList[A]] = RegexParser.ManyNonEmpty(this)

    final def present: RegexParser[Boolean] = this.?.map(_.nonEmpty)

    final def map[B](f: A => B): RegexParser[B] = RegexParser.Mapped(this, f)
    final def mapOrFail[B](f: A => Either[String, B]): RegexParser[B] = RegexParser.MappedOrFail(this, f)
    final def as[B](f: => B): RegexParser[B] = this.map { _ => f }
    final def validate(error: A => Option[String]): RegexParser[A] = this.mapOrFail { a => error(a).toLeft(a) }

  }
  private object RegexParser {

    extension [A](self: => RegexParser[A]) {

      def suspend: RegexParser[A] = RegexParser.Suspend(Lazy { self })

      def >>>[B](that: => RegexParser[B])(using zip: Zip[A, B]): RegexParser[zip.Out] = RegexParser.Then(self.suspend, that.suspend, zip)

      def |[B >: A](that: => RegexParser[B]): RegexParser[B] = RegexParser.Or(self.suspend, that.suspend)

    }

    sealed trait ParseResult[+A]
    object ParseResult {
      final case class Success[+A](value: A, remainingIndex: Int) extends ParseResult[A]
      sealed trait NonSuccess extends ParseResult[Nothing]
      final case class Failure(error: String) extends ParseResult.NonSuccess
      final case class Unknown(consumed: Boolean, remainingIndex: Int) extends ParseResult.NonSuccess
    }

    final case class Suspend[A](inner: Lazy[RegexParser[A]]) extends RegexParser[A] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[A] = inner.value.parse(chars, index)

    }

    final case class Optional[A](a: RegexParser[A]) extends RegexParser[Option[A]] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[Option[A]] =
        a.parse(chars, index) match {
          case ParseResult.Success(value, remainingIndex) => ParseResult.Success(value.some, remainingIndex)
          case res: ParseResult.Failure                   => res
          case _: ParseResult.Unknown                     => ParseResult.Success(None, index)
        }

    }

    final case class Then[A, B, C](a: RegexParser[A], b: RegexParser[B], zip: Zip.Out[A, B, C]) extends RegexParser[C] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[C] =
        a.parse(chars, index) match {
          case ParseResult.Success(aValue, aRemaining) =>
            b.parse(chars, aRemaining) match {
              case ParseResult.Success(bValue, bRemaining) => ParseResult.Success(zip.zip(aValue, bValue), bRemaining)
              case res: ParseResult.Failure                => res
              case ParseResult.Unknown(_, remainingIndex)  => ParseResult.Unknown(true, remainingIndex)
            }
          case res: ParseResult.NonSuccess => res
        }

    }

    final case class Or[A](a: RegexParser[A], b: RegexParser[A]) extends RegexParser[A] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[A] =
        a.parse(chars, index) match {
          case success: ParseResult.Success[A] => success
          case res: ParseResult.Failure        => res
          case _: ParseResult.Unknown          => b.parse(chars, index) // TODO (KR) : take best
        }

    }

    final case class Many[A](a: RegexParser[A]) extends RegexParser[List[A]] {

      @tailrec
      private def loop(chars: IArray[Char], index: Int, rStack: List[A]): ParseResult[List[A]] =
        a.parse(chars, index) match {
          case ParseResult.Success(value, remainingIndex) => loop(chars, remainingIndex, value :: rStack)
          case res: ParseResult.Failure                   => res
          case _: ParseResult.Unknown                     => ParseResult.Success(rStack.reverse, index)
        }

      override def parse(chars: IArray[Char], index: Int): ParseResult[List[A]] =
        loop(chars, index, Nil)

    }

    final case class ManyNonEmpty[A](a: RegexParser[A]) extends RegexParser[NonEmptyList[A]] {

      @tailrec
      private def loop(chars: IArray[Char], index: Int, rStack: List[A]): ParseResult[NonEmptyList[A]] =
        a.parse(chars, index) match {
          case ParseResult.Success(value, remainingIndex) => loop(chars, remainingIndex, value :: rStack)
          case res: ParseResult.Failure                   => res
          case res: ParseResult.Unknown                   =>
            NonEmptyList.fromList(rStack.reverse) match
              case Some(value) => ParseResult.Success(value, index)
              case None        => res
        }

      override def parse(chars: IArray[Char], index: Int): ParseResult[NonEmptyList[A]] =
        loop(chars, index, Nil)

    }

    final case class Mapped[A, B](a: RegexParser[A], ab: A => B) extends RegexParser[B] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[B] =
        a.parse(chars, index) match {
          case ParseResult.Success(value, remainingIndex) => ParseResult.Success(ab(value), remainingIndex)
          case res: ParseResult.NonSuccess                => res
        }

    }

    final case class FlatMapped[A, B](a: RegexParser[A], ab: A => RegexParser[B]) extends RegexParser[B] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[B] =
        a.parse(chars, index) match {
          case ParseResult.Success(value, remainingIndex) => ab(value).parse(chars, remainingIndex)
          case res: ParseResult.NonSuccess                => res
        }

    }

    final case class MappedOrFail[A, B](a: RegexParser[A], ab: A => Either[String, B]) extends RegexParser[B] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[B] =
        a.parse(chars, index) match {
          case ParseResult.Success(value, remainingIndex) =>
            ab(value) match {
              case Right(value) => ParseResult.Success(value, remainingIndex)
              case Left(error)  => ParseResult.Failure(error)
            }
          case res: ParseResult.NonSuccess => res
        }

    }

    final case class ExactChar(char: Char) extends RegexParser[Unit] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[Unit] =
        chars.lift(index) match {
          case Some(`char`) => ParseResult.Success((), index + 1)
          case _            => ParseResult.Unknown(false, index)
        }

    }

    final case class CharIn(set: InfiniteSet[Char]) extends RegexParser[Char] {

      override def parse(chars: IArray[Char], index: Int): ParseResult[Char] =
        chars.lift(index) match {
          case Some(char) if set.contains(char) => ParseResult.Success(char, index + 1)
          case _                                => ParseResult.Unknown(false, index)
        }

    }

  }

  private object parsers {

    val escapedCharOrCharClass: RegexParser[Char | CharClass] =
      RegexParser.ExactChar('\\') >>> RegexParser.CharIn(InfiniteSet.full).map { // TODO (KR) : support other char classes
        case 'n' => '\n'
        case 'd' => CharClass.inclusiveRange('0', '9')
        case c   => c
      }

    val charClass: RegexParser[CharClass] = {
      val nonEscapedCharOrCharClass: RegexParser[Char | CharClass] =
        RegexParser.CharIn(InfiniteSet.Exclusive('[', ']', '^', '-', '\\')).map {
          case '.' => CharClass.anything
          case c   => c
        }

      val charOrCharClass: RegexParser[Char | CharClass] = escapedCharOrCharClass | nonEscapedCharOrCharClass

      val pair: RegexParser[CharClass] =
        (charOrCharClass >>> (RegexParser.ExactChar('-') >>> charOrCharClass).?).mapOrFail {
          case (c: Char, None)            => CharClass.inclusive(c).asRight
          case (c: CharClass, None)       => c.asRight
          case (c1: Char, Some(c2: Char)) => CharClass.inclusiveRange(c1, c2).asRight
          case (c1, Some(c2))             => s"Invalid char-class range: $c1, $c2".asLeft
        }

      (RegexParser.ExactChar('[') >>> RegexParser.ExactChar('^').present >>> pair.+ >>> RegexParser.ExactChar(']')).map {
        case (true, elems)  => !elems.reduceLeft { _ | _ }
        case (false, elems) => elems.reduceLeft { _ | _ }
      }
    }

    val quant: RegexParser[Quant] = {
      val int: RegexParser[Int] = RegexParser.CharIn(InfiniteSet.Inclusive('0'.to('9').toSet)).+.mapOrFail(_.mkString.toIntOption.toRight("invalid int?"))

      val simple: RegexParser[Quant] =
        RegexParser.ExactChar('?').as { Quant.? } |
          RegexParser.ExactChar('*').as { Quant.* } |
          RegexParser.ExactChar('+').as { Quant.+ }

      val bracketWithMin: RegexParser[Quant] =
        (int >>> (RegexParser.ExactChar(',') >>> int.?).?).map {
          case (min, Some(Some(max))) => Quant.Between(min, max)
          case (min, Some(None))      => Quant.AtLeast(min)
          case (exact, None)          => Quant.Exactly(exact)
        }

      val bracketWithoutMin: RegexParser[Quant] =
        (RegexParser.ExactChar(',') >>> int).map(Quant.Between(0, _))

      simple | (RegexParser.ExactChar('{') >>> (bracketWithMin | bracketWithoutMin) >>> RegexParser.ExactChar('}'))
    }

    lazy val singleElem: RegexParser[ParsedRegex] = {
      val nonEscapedCharOrCharClass: RegexParser[Char | CharClass] =
        RegexParser.CharIn(InfiniteSet.Exclusive('[', '^', '$', '\\', '(', ')', '|', '?', '*', '+', '{')).map {
          case '.' => CharClass.anything
          case c   => c
        }

      val charElem: RegexParser[CharClass] =
        (escapedCharOrCharClass | nonEscapedCharOrCharClass).map {
          case c: Char      => CharClass.inclusive(c)
          case c: CharClass => c
        }

      ((charClass | charElem | wrappedGroup).map(_.simplify) >>> quant.?).map {
        case (reg, None)    => reg
        case (reg, Some(q)) => WithQuant(reg, q)
      }
    }

    lazy val sequence: RegexParser[Sequence] =
      singleElem.*.map { elems => Sequence.ofElems(elems*) }

    lazy val unwrappedGroup: RegexParser[Group] =
      (sequence >>> (RegexParser.ExactChar('|') >>> sequence).*).map { case (h, t) => Group(NonEmptyList(h, t)) }

    lazy val wrappedGroup: RegexParser[Group] =
      RegexParser.ExactChar('(') >>> unwrappedGroup >>> RegexParser.ExactChar(')')

    val regex: RegexParser[ParsedRegex] = unwrappedGroup.map(_.simplify)

  }

  def parse(regex: String): Either[String, ParsedRegex] = {
    val chars: IArray[Char] = IArray.unsafeFromArray(regex.toCharArray)
    parsers.regex.parse(chars, 0) match {
      case RegexParser.ParseResult.Success(value, remainingIndex) if remainingIndex == chars.length => value.asRight
      case RegexParser.ParseResult.Success(_, remainingIndex)                                       => s"Unable to continue parsing at index $remainingIndex (success)".asLeft
      case RegexParser.ParseResult.Unknown(_, remainingIndex)                                       => s"Unable to continue parsing at index $remainingIndex (unknown)".asLeft
      case RegexParser.ParseResult.Failure(error)                                                   => error.asLeft
    }
  }

}
