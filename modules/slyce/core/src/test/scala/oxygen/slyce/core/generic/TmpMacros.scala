package oxygen.slyce.core.generic

import oxygen.slyce.core.*

// FIX-PRE-MERGE (KR) : remove
object TmpMacros {

  sealed trait Value extends Token

  @regex("[A-Za-z_][A-Za-z_0-9]*".r)
  final case class Variable() extends Value

  @regex("-?[0-9]+".r)
  final case class Literal(int: Int) extends Value
  object Literal {
    given Token.Builder[Literal] = str => str.toIntOption.map(Literal(_)).toRight(s"not a valid int: $str")
  }

  sealed trait Op extends Token

  sealed trait AddOp extends Op

  @regex("\\+".r)
  final case class PlusOp() extends AddOp

  @regex("-".r)
  final case class MinusOp() extends AddOp

  sealed trait MultOp extends Op

  @regex("\\*".r)
  final case class TimesOp() extends MultOp

  @regex("/".r)
  final case class DivideOp() extends MultOp

  @regex("\\^".r)
  final case class PowOp() extends Op

  @regex(":=".r)
  final case class SetVal() extends Token

  @regex("@show".r)
  final case class ShowAnnot() extends Token

  @regex("_".r)
  final case class Underscore() extends Token

  @regex("\\(".r)
  final case class OpenParen() extends Token

  @regex("\\)".r)
  final case class CloseParen() extends Token

  sealed trait Expr1 extends Node
  object Expr1 {

    final case class A(a: Expr2, b: MultOp, c: Expr1) extends Expr1
    final case class B(v: Expr2) extends Expr1

  }

  sealed trait Expr2 extends Node
  object Expr2 {

    final case class A(a: Expr2, b: MultOp, c: Expr3) extends Expr2
    final case class B(v: Expr3) extends Expr2

  }

  sealed trait Expr3 extends Node
  object Expr3 {

    final case class A(a: Expr3, b: AddOp, c: Expr4) extends Expr3
    final case class B(v: Expr4) extends Expr3

  }

  sealed trait Expr4 extends Node
  object Expr4 {

    final case class A(a: OpenParen, b: Expr1, c: CloseParen) extends Expr4
    final case class B(a: Value) extends Expr4

  }

  final case class Defn(show: Option[ShowAnnot], v: Variable, set: SetVal, res: Expr1 | Underscore) extends Node

  final case class Program(defns: List[Defn], res: Expr1) extends Node

  Macros.showStuff[Program]

  // FIX-PRE-MERGE (KR) :

}
