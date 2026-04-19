package oxygen.quoted

import scala.quoted.*

def compileTimeEvalEither[A: {Type, FromExpr}, B: {Type, ToExpr as outToExpr}](expr: Expr[A])(decode: A => Either[String, B])(using Quotes): Expr[B] = {
  val inValue: A = expr.evalRequired("value must be known at compile time")
  decode(inValue) match
    case Right(outValue) => outToExpr(outValue)
    case Left(error)     => report.errorAndAbort(s"detected bad value at compile time : $error", expr)
}

def compileTimeEvalOption[A: {Type, FromExpr}, B: {Type, ToExpr as outToExpr}](expr: Expr[A])(decode: A => Option[B])(using Quotes): Expr[B] = {
  val inValue: A = expr.evalRequired("value must be known at compile time")
  decode(inValue) match
    case Some(outValue) => outToExpr(outValue)
    case None           => report.errorAndAbort(s"detected bad value at compile time", expr)
}
