package oxygen.slyce.core.generic

import oxygen.slyce.core.*

// FIX-PRE-MERGE (KR) : remove
object TmpMacros {

  sealed trait Value extends Token
  final case class Variable() extends Value
  final case class Literal() extends Value

  sealed trait Op extends Token

  sealed trait AddOp extends Op
  final case class PlusOp() extends AddOp
  final case class MinusOp() extends AddOp

  sealed trait MultOp extends Op
  final case class TimesOp() extends MultOp
  final case class DivideOp() extends MultOp

  final case class PowOp() extends Op

  final case class SetVal() extends Token
  final case class OpenParen() extends Token
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

  final case class Defn(v: Variable, set: SetVal, res: Expr1) extends Node

  final case class Program(defns: List[Defn], res: Expr1) extends Node

  Macros.showStuff[Program]

  // FIX-PRE-MERGE (KR) :

}
