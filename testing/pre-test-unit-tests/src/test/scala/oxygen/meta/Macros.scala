package oxygen.meta

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

object Macros {

  private def seqFunImpl[S[_]: Type, A: Type](values: Expr[Seq[A]])(using Quotes): Expr[S[A]] =
    Varargs.unapply(values).getOrElse(report.errorAndAbort("not varargs")).unsafeSeqToExprOf[S]

  inline def seqFun[S[_], A](inline values: A*): S[A] = ${ seqFunImpl[S, A]('values) }

  private def stringMatchImpl(str1: Expr[String], str2: Expr[String])(using Quotes): Expr[String] = {
    val mat: MatchBuilder[(String, String), String] =
      (CaseExtractor.const[String](Expr("const")) ++ CaseExtractor.const[String](Expr("const"))).withRHS[String] { _ => Expr("<const>") } ||
        (CaseExtractor.const[String](Expr("const")) ++ CaseExtractor.extract[String]("s")).withRHS[String] { str => str } ||
        (CaseExtractor.extract[String]("s") ++ CaseExtractor.const[String](Expr("const"))).withRHS[String] { str => str } ||
        (CaseExtractor.extract[String]("s1") ++ CaseExtractor.extract[String]("s2")).withRHS[String] { case (a, b) => '{ s"${$a} + ${$b}" } }

    val res = mat.withNonExhaustive.matchOn(str1, str2)

    // report.info(res.toTerm.show(using Printer.TreeAnsiCode))

    res
  }

  inline def stringMatch(str1: String, str2: String): String = ${ stringMatchImpl('str1, 'str2) }

  private def defaultArgsImpl[A: Type](using Quotes): Expr[String] = {
    val g = K0.ProductGeneric.of[A]

    g.mapChildren
      .map {
        [b] =>
          (_, _) ?=>
            (field: g.Field[b]) =>
              field.constructorDefault.map { default =>
                Growable(
                  Expr(field.name),
                  Expr(" = "),
                  '{ $default.toString },
                )
            }
      }
      .flattenOpt
      .surround(Growable(Expr("[")), Growable(Expr(", ")), Growable(Expr("]")))
      .flatten
      .exprMkString
  }

  inline def defaultArgs[A]: String = ${ defaultArgsImpl[A] }

}
