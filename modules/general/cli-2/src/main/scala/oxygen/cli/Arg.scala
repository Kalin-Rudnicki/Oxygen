package oxygen.cli

sealed trait Arg {
  val idx: Int
}

final case class PositionalArg(idx: Int, value: String) extends Arg

sealed trait NamedArg extends Arg

final case class LongNameArg(idx: Int, name: String, nested: Any) extends NamedArg

final case class ShortNameArg(idx: Int, name: Char, nested: Any) extends NamedArg

final case class MultiShortNameArg(idx: Int, subIdx: Int, name: Char) extends NamedArg
