package oxygen.quoted

import oxygen.quoted.companion.PrinterCompanion
import scala.quoted.*
import scala.reflect.ClassTag

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Expr
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension [A](self: Expr[A]) {

  def toTerm(using quotes: Quotes): Term =
    Term.wrap(quotes.reflect.asTerm(self))

  def showWith(f: PrinterCompanion => Printer[Tree])(using Quotes): String = self.toTerm.showWith(f)
  def showCode(using Quotes): String = self.toTerm.showCode
  def showShortCode(using Quotes): String = self.toTerm.showShortCode
  def showAnsiCode(using Quotes): String = self.toTerm.showAnsiCode
  def showStructure(using Quotes): String = self.toTerm.showStructure

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, return None.
    */
  def evalOption(using from: FromExpr[A], quotes: Quotes): Option[A] =
    from.unapply(self)

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, die.
    */
  def evalRequired(msg: String)(using from: FromExpr[A], quotes: Quotes): A =
    self.evalOption.getOrElse(report.errorAndAbort(s"Unable to extract Expr, msg = $msg\nexpr:\n${self.showAnsiCode}"))

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, die.
    */
  def evalRequired(using from: FromExpr[A], quotes: Quotes): A =
    self.evalOption.getOrElse(report.errorAndAbort(s"Unable to extract Expr.\nexpr:\n${self.showAnsiCode}"))

  /**
    * Attempt to convert this Expr[A] into an A.
    * If it does not succeed, return a Left of the original expr.
    */
  def evalEither(using from: FromExpr[A], quotes: Quotes): Either[Expr[A], A] =
    self.evalOption.toRight(self)

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Type
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension (self: Type[?]) {

  def toTypeRepr(using quotes: Quotes): TypeRepr =
    TypeRepr.fromType(self)

}

extension (self: Type.type) {

  def unwrap1[T[_], T1](using Type[T[T1]], Quotes): (Type[T], Type[T1]) = TypeRepr.of[T[T1]] match
    case AppliedType(tRepr, List(t1Repr)) => (tRepr.asTypeOf[T], t1Repr.asTypeOf[T1])
    case repr                             => report.errorAndAbort(s"Unable to extract T[T1] type param for: ${repr.showAnsiCode}")

  def unwrap2[T[_, _], T1, T2](using Type[T[T1, T2]], Quotes): (Type[T], Type[T1], Type[T2]) = TypeRepr.of[T[T1, T2]] match
    case AppliedType(tRepr, List(t1Repr, t2Repr)) => (tRepr.asTypeOf[T], t1Repr.asTypeOf[T1], t2Repr.asTypeOf[T2])
    case repr                                     => report.errorAndAbort(s"Unable to extract T[T1, T2] type params for: ${repr.showAnsiCode}")

  def unwrap3[T[_, _, _], T1, T2, T3](using Type[T[T1, T2, T3]], Quotes): (Type[T], Type[T1], Type[T2], Type[T3]) = TypeRepr.of[T[T1, T2, T3]] match
    case AppliedType(tRepr, List(t1Repr, t2Repr, t3Repr)) => (tRepr.asTypeOf[T], t1Repr.asTypeOf[T1], t2Repr.asTypeOf[T2], t3Repr.asTypeOf[T3])
    case repr                                             => report.errorAndAbort(s"Unable to extract T[T1, T2, T3] type params for: ${repr.showAnsiCode}")

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Model
//////////////////////////////////////////////////////////////////////////////////////////////////////

extension [T <: Model](self: T) {

  def narrow[T2 <: T](hint: String)(using ct: ClassTag[T2]): T2 = self match
    case ct(t2) => t2
    case _      => report.companion(using self.quotes).errorAndAbort(s"Not a '${ct.runtimeClass.getName}': ${self.getClass.getName}\n($hint)\n\n$self", self.maybePos)

  def narrow[T2 <: T](using ct: ClassTag[T2]): T2 = self match
    case ct(t2) => t2
    case _      => report.companion(using self.quotes).errorAndAbort(s"Not a '${ct.runtimeClass.getName}': ${self.getClass.getName}\n\n$self", self.maybePos)

  def narrowOpt[T2 <: T](using ct: ClassTag[T2]): Option[T2] =
    ct.unapply(self)

}
