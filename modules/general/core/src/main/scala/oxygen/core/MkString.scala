package oxygen.core

import oxygen.core.typeclass.SeqOps

sealed trait MkString {

  // FIX-PRE-MERGE (KR) :

}
object MkString {

  val empty: MkString = MkString.impl.Empty

  def fromAny(value: Any): MkString = value.asInstanceOf[Matchable] match
    case null            => MkString.impl.Empty
    case value: MkString => value
    case value: String   => if value.nonEmpty then MkString.impl.Str(value) else MkString.impl.Empty

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impl
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object impl {

    case object Empty extends MkString

    final case class Str(value: String) extends MkString

    final case class Interpolated(strings: IArray[String], args: IArray[MkString]) extends MkString

    final case class CustomIndented(underlying: MkString, indent: MkString) extends MkString

    final case class DefaultIndented(underlying: MkString) extends MkString

    final case class Colorized(underlying: MkString, fg: Specified[Color], bg: Specified[Color]) extends MkString

    final case class Concat(first: MkString, second: MkString) extends MkString
    final case class ConcatAll[S[_]](seqOps: SeqOps[S], underlying: S[MkString]) extends MkString

    sealed trait SeqJoin extends MkString
    object SeqJoin {
      final case class Simple[S[_]](seqOps: SeqOps[S], underlying: S[Any]) extends SeqJoin
      final case class Case2[S[_]](seqOps: SeqOps[S], underlying: S[Any], join: String) extends SeqJoin
      final case class Case3[S[_]](seqOps: SeqOps[S], underlying: S[Any], prefix: String, join: String, after: String) extends SeqJoin
    }

  }

}
