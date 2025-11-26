package oxygen.sql.generic.parsing

import oxygen.meta.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] trait TermTransformer {

  def simplified: TermTransformer.Simplified

  final def >>>(that: TermTransformer): TermTransformer.Simplified = (this.simplified, that.simplified) match
    case (a: TermTransformer.Transform, b: TermTransformer.Transform) => TermTransformer.AndThen(a, b)
    case (TermTransformer.Die, _)                                     => TermTransformer.Die
    case (_, TermTransformer.Die)                                     => TermTransformer.Die
    case (TermTransformer.Id, b)                                      => b
    case (a, TermTransformer.Id)                                      => a

}
private[generic] object TermTransformer {

  sealed trait Simplified extends TermTransformer {

    override final def simplified: Simplified = this

    def convertTerm(fInTerm: Term)(using Quotes): Term

    final def convertExpr[A: Type, B: Type](in: Expr[A])(using Quotes): Expr[B] =
      convertTerm(in.toTerm).asExprOf[B]

    final def convertExprF[A: Type, B: Type](using Quotes): Expr[A => B] =
      '{ (in: A) => ${ convertExpr[A, B]('in) } }

  }

  sealed trait SimpleValid extends Simplified

  trait Defer extends TermTransformer {
    protected def defer: TermTransformer
    override final def simplified: Simplified = defer.simplified
  }

  trait DeferToParam extends Defer {
    val param: Function.NamedParam
    override protected final def defer: TermTransformer = param
  }

  trait Die extends TermTransformer {
    override final def simplified: Simplified = Die
  }
  case object Die extends TermTransformer.Simplified {

    override def convertTerm(fInTerm: Term)(using Quotes): Term =
      report.errorAndAbort("Term transformation not allowed. Expected not to be called...")

  }

  trait Id extends TermTransformer {
    override final def simplified: Simplified = Id
  }
  case object Id extends TermTransformer.SimpleValid {

    override def convertTerm(fInTerm: Term)(using Quotes): Term = fInTerm

  }

  trait Transform extends TermTransformer.SimpleValid {

    def inTpe: TypeRepr
    def outTpe: TypeRepr
    protected def convertTermInternal(term: Term)(using Quotes): Term

    override final def convertTerm(fInTerm: Term)(using Quotes): Term = {
      val fInTpe: TypeRepr = fInTerm.tpe.widen

      if (!(fInTpe <:< inTpe))
        report.errorAndAbort(
          s"""AccessibleParam.getExpr received bad input.
             |Expected input type: ${inTpe.showAnsiCode}
             |Actual input type:   ${fInTpe.showAnsiCode}
             |expr:
             |${fInTerm.showAnsiCode}""".stripMargin,
        )

      val fOutTerm: Term = convertTermInternal(fInTerm)
      val fOutTpe: TypeRepr = fOutTerm.tpe.widen

      if (!(outTpe <:< fOutTpe))
        report.errorAndAbort(
          s"""AccessibleParam.getExpr generated bad output.
             |Expected output type: ${outTpe.showAnsiCode}
             |Actual output type:   ${fOutTpe.showAnsiCode}
             |expr:
             |${fOutTerm.showAnsiCode}""".stripMargin,
        )

      fOutTerm
    }

  }

  final case class FromSelect(sel: Select) extends Transform {
    override def inTpe: TypeRepr = sel.qualifier.tpe.widen
    override def outTpe: TypeRepr = sel.tpe.widen
    override protected def convertTermInternal(term: Term)(using Quotes): Term = term.select(sel.symbol)
  }

  final case class FromProductGenericField(g: K0.ProductGeneric[?], field: g.Field[?]) extends Transform {
    override def inTpe: TypeRepr = g.typeRepr.widen
    override def outTpe: TypeRepr = field.typeRepr.widen
    override protected def convertTermInternal(term: Term)(using Quotes): Term = field.fromParentTerm(term)
  }

  final case class AndThen(a: Transform, b: Transform) extends Transform {
    override def inTpe: TypeRepr = a.inTpe
    override def outTpe: TypeRepr = b.outTpe
    override protected def convertTermInternal(term: Term)(using Quotes): Term = b.convertTerm(a.convertTerm(term))
  }

}
