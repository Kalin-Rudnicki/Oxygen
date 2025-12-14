package oxygen.meta

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.experimental
import scala.quoted.*

@experimental
trait ExampleTrait {

  def upper: String
  def lower: String

  def defDef(value: String): Boolean

}
object ExampleTrait {

  private def implImpl(strExpr: Expr[String])(using quotes: Quotes): Expr[ExampleTrait] = {

    val exampleTraitTypeRepr = TypeRepr.of[ExampleTrait]
    val sym = exampleTraitTypeRepr.classSymbol.get

    val upperSym: Symbol = sym.declaredMethod("upper").head
    val lowerSym: Symbol = sym.declaredMethod("lower").head
    val defDefSym: Symbol = sym.declaredMethod("defDef").head

    val upperDefDef: DefDef = upperSym.tree.narrow[DefDef]
    val lowerDefDef: DefDef = lowerSym.tree.narrow[DefDef]
    val defDefDefDef: DefDef = defDefSym.tree.narrow[DefDef]

    val appliedClassDefTerm: Term =
      ClassDef
        .newClassInstantiated(Symbol.spliceOwner, "ExampleTrait_Impl", List(TypeRepr.of[Object], exampleTraitTypeRepr), None) { clsSym =>
          given Quotes = clsSym.asQuotes

          val privateSym: Symbol = Symbol.newVal(clsSym, "internalString", TypeRepr.of[String], Flags.Private, clsSym)
          val newUpperSym: Symbol = Symbol.newVal(clsSym, upperSym.name, upperDefDef.methodType, upperSym.flags | Flags.Override, Symbol.noSymbol)
          val newLowerSym: Symbol = Symbol.newVal(clsSym, lowerSym.name, lowerDefDef.methodType, lowerSym.flags | Flags.Override, Symbol.noSymbol)
          val newDefDefSym: Symbol = Symbol.newMethod(clsSym, defDefSym.name, defDefDefDef.methodType, defDefSym.flags | Flags.Override, Symbol.noSymbol)

          extension (self: Symbol)
            def implementStr(expr: Expr[String]): (Symbol, ValDef) =
              self -> ValDef.companion.apply(self, expr.toTerm.changeOwner(self).some)

          extension (self: Symbol)
            def implementDef(pf: PartialFunction[List[List[Tree]], Term]): (Symbol, DefDef) =
              self ->
                DefDef.companion.apply(
                  self,
                  args => pf.lift(args).getOrElse { report.errorAndAbort(s"unable to match on input params (${self.name}): $args") }.some,
                )

          List(
            privateSym.implementStr { strExpr },
            newUpperSym.implementDef { case Nil :: Nil => '{ ${ privateSym.toTerm.asExprOf[String] }.toUpperCase }.toTerm },
            newLowerSym.implementDef { case Nil :: Nil => '{ ${ privateSym.toTerm.asExprOf[String] }.toLowerCase }.toTerm },
            newDefDefSym.implementDef { case List(List(t: Term)) =>
              '{ ${ newLowerSym.toTerm.asExprOf[String] } == ${ t.asExprOf[String] } || ${ newUpperSym.toTerm.asExprOf[String] } == ${ t.asExprOf[String] } }.toTerm
            },
          )
        }

    report.errorAndAbort(appliedClassDefTerm.showAnsiCode)

    appliedClassDefTerm.asExprOf[ExampleTrait]
  }

  inline def impl(str: String): ExampleTrait = ${ implImpl('str) }

}
