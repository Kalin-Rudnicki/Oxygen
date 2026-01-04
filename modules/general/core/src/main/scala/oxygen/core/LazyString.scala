package oxygen.core

import oxygen.core.typeclass.SeqOps

sealed trait LazyString {

  // protected def writeSimple(builder: StringBuilder)

}
object LazyString {

  val empty: LazyString = LazyString.impl.Empty

  def fromString(value: Any): LazyString =
    ??? // FIX-PRE-MERGE (KR) :

  def fromAny(value: Any): LazyString = value.asInstanceOf[Matchable] match
    case null              => LazyString.impl.Empty
    case value: LazyString => value
    case value: String     => fromString(value)
    case _                 => fromString(value.toString)

  // FIX-PRE-MERGE (KR) :
  enum OptimizeFor { case Small, Large, Formatting }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impl
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object impl {

    case object Empty extends LazyString

    case object Newline extends LazyString

    final case class Str(value: String) extends LazyString

    final case class Interpolated(strings: IArray[String], args: IArray[LazyString]) extends LazyString

    final case class CustomIndented(underlying: LazyString, indent: LazyString) extends LazyString

    final case class DefaultIndented(underlying: LazyString) extends LazyString

    final case class Colorized(underlying: LazyString, fg: Specified[Color], bg: Specified[Color]) extends LazyString

    final case class Concat(first: LazyString, second: LazyString) extends LazyString
    final case class ConcatAll[S[_]](seqOps: SeqOps[S], underlying: S[LazyString]) extends LazyString

    sealed trait SeqJoin extends LazyString
    object SeqJoin {
      final case class Simple[S[_]](seqOps: SeqOps[S], underlying: S[Any]) extends SeqJoin
      final case class Case2[S[_]](seqOps: SeqOps[S], underlying: S[Any], join: String) extends SeqJoin
      final case class Case3[S[_]](seqOps: SeqOps[S], underlying: S[Any], prefix: String, join: String, after: String) extends SeqJoin
    }

  }

}
