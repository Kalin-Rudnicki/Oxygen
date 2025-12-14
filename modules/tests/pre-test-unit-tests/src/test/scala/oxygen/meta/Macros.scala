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
      .map { [b] => (_, _) ?=> (field: g.Field[b]) =>
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

  // @K0.annotation.showDerivation[ToExprT]
  final case class InnerExample(c: Char) derives ToExprT, FromExprT

  // @K0.annotation.showDerivation[FromExprT]
  final case class ProductToExprExample(a: String, b: Boolean, c: Option[InnerExample]) derives ToExprT, FromExprT

  private def productToExprExampleImpl(aExpr: Expr[String], bExpr: Expr[Boolean], cExpr: Expr[Char])(using Quotes): Expr[ProductToExprExample] = {
    val a: String = aExpr.evalRequired
    val b: Boolean = bExpr.evalRequired
    val c: Char = cExpr.evalRequired
    val inst: ProductToExprExample = ProductToExprExample(a, b, InnerExample(c).some)
    Expr { inst }
  }

  inline def productToExprExample(a: String, b: Boolean, c: Char): ProductToExprExample = ${ productToExprExampleImpl('a, 'b, 'c) }

  // @K0.annotation.showDerivation[FromExprT]
  enum SumToExprExample derives ToExprT, FromExprT {
    case Case1(a: String, b: Option[Boolean], c: InnerExample)
    case Case2(c: Int)
    case Case3
  }

  private def sumToExprExampleImpl(aExpr: Expr[String], bExpr: Expr[Boolean], cExpr: Expr[Char], dExpr: Expr[Int])(using Quotes): Expr[(SumToExprExample, SumToExprExample, SumToExprExample)] = {
    val a: String = aExpr.evalRequired
    val b: Boolean = bExpr.evalRequired
    val c: Char = cExpr.evalRequired
    val d: Int = dExpr.evalRequired
    val inst1: SumToExprExample = SumToExprExample.Case1(a, b.some, InnerExample(c))
    val inst2: SumToExprExample = SumToExprExample.Case2(d)
    val inst3: SumToExprExample = SumToExprExample.Case3
    Expr { (inst1, inst2, inst3) }
  }

  inline def sumToExprExample(a: String, b: Boolean, c: Char, d: Int): (SumToExprExample, SumToExprExample, SumToExprExample) = ${ sumToExprExampleImpl('a, 'b, 'c, 'd) }

  private def productRoundTripImpl(tmp1: Expr[ProductToExprExample])(using Quotes): Expr[ProductToExprExample] = {
    val tmp2: ProductToExprExample = tmp1.evalRequired
    val tmp3: Expr[ProductToExprExample] = Expr(tmp2)
    tmp3
  }

  inline def productRoundTrip(inline a: ProductToExprExample): ProductToExprExample = ${ productRoundTripImpl('a) }

  private def sumRoundTripImpl(tmp1: Expr[SumToExprExample])(using Quotes): Expr[SumToExprExample] = {
    val tmp2: SumToExprExample = tmp1.evalRequired
    val tmp3: Expr[SumToExprExample] = Expr(tmp2)
    tmp3
  }

  inline def sumRoundTrip(inline a: SumToExprExample): SumToExprExample = ${ sumRoundTripImpl('a) }

  private def myCachedFunction[A: Type, B: Type](f: Expr[A => B])(using Quotes): Expr[A => B] =
    '{
      val cache: scala.collection.mutable.Map[A, B] = scala.collection.mutable.Map.empty
      { (a: A) =>
        println(s"Searching for: $a")
        cache.get(a) match {
          case Some(value) =>
            println(s"hit: $a = $value")
            value
          case None =>
            println(s"miss: $a")
            val value: B = $f(a)
            cache.update(a, value)
            value
        }
      }
    }

  inline def cachedFunction[A, B](inline f: A => B): A => B = ${ myCachedFunction[A, B]('f) }

}
