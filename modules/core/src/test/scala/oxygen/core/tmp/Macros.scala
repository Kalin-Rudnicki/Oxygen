package oxygen.core.tmp

import scala.quoted.*

object Macros {

  inline def myMacro[A]: Unit = ${ myMacroImpl[A] }

  @scala.annotation.nowarn
  private def myMacroImpl[A](using quotes: Quotes, tpe: Type[A]): Expr[Unit] = {
    import quotes.reflect.*

    object unapplies {

      object caseClass {

        def unapply(sym: Symbol): Boolean =
          sym.flags.is(Flags.Case) && !sym.flags.is(Flags.Module)

      }

      object caseObject {

        def unapply(sym: Symbol): Boolean =
          sym.flags.is(Flags.Case) && sym.flags.is(Flags.Module)

      }

    }

    val typeRepr = TypeRepr.of[A]
    val typeSymbol = typeRepr.typeSymbol

    val isCaseObject: Boolean =
      typeSymbol match {
        case _ @unapplies.caseClass()  => false
        case _ @unapplies.caseObject() => true
        case _                         => report.errorAndAbort(s"Type ${typeRepr.show} is not a `case object` or `case class`")
      }

    val primaryConstructor = typeSymbol.primaryConstructor

    val (typeSymbols0, _, typeArgs): (List[Symbol], List[Symbol], List[TypeRepr]) =
      (typeRepr, primaryConstructor.paramSymss) match {
        case (_, syms :: Nil)                                                       => (Nil, syms, Nil)
        case (at: AppliedType, tsyms :: syms :: Nil) if tsyms.forall(_.isTypeParam) => (tsyms, syms, at.args)
        case (_, symss) =>
          val msg =
            s"""Type ${typeRepr.show} has non-single constructor arg groups.
                 |    allowed: MyCaseClass(...)
                 |not allowed: MyCaseClass(...)(...)
                 |not allowed: MyCaseClass(...)(...)(...)
                 |
                 |params:${symss.map { s => s"\n    - ${s.mkString(", ")}" }.mkString}""".stripMargin
          report.errorAndAbort(msg)
      }
    val typeSymbols: List[Symbol] = typeSymbols0.map(s => typeSymbol.typeMember(s.name))

    val caseFieldSymbols: List[Symbol] = typeRepr.typeSymbol.caseFields
    val caseFieldValDefs: List[ValDef] =
      caseFieldSymbols.map {
        _.tree match {
          case valDef: ValDef => valDef
          case _              => report.errorAndAbort("???")
        }
      }

    println(caseFieldValDefs)

    '{ () }
  }

}
