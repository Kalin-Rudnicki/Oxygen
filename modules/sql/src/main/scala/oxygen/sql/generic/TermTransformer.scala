package oxygen.sql.generic

import oxygen.quoted.*
import scala.quoted.*

private[generic] trait TermTransformer {

  def inTpe: TypeRepr
  def outTpe: TypeRepr

  protected def convertTermInternal(term: Term)(using Quotes): Term
  final def convertTerm(fInTerm: Term)(using Quotes): Term = {
    val fInTpe: TypeRepr = fInTerm.tpe

    if (!(fInTpe <:< inTpe))
      report.errorAndAbort(
        s"""AccessibleParam.getExpr received bad input.
             |Expected input type: ${inTpe.showAnsiCode}
             |Actual input type:   ${fInTpe.showAnsiCode}
             |expr:
             |${fInTerm.showAnsiCode}""".stripMargin,
      )

    val fOutTerm: Term = convertTerm(fInTerm)
    val fOutTpe: TypeRepr = fOutTerm.tpe

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

  final def getExpr[Out: Type](in: Expr[?])(using Quotes): Expr[Out] =
    convertTerm(in.toTerm).asExprOf[Out]

  final def >>>(that: TermTransformer): TermTransformer = (this, that) match
    case (_: TermTransformer.Id, _) => that
    case (_, _: TermTransformer.Id) => this
    case _                          => TermTransformer.AndThen(this, that)

}
object TermTransformer {

  trait Id extends TermTransformer {
    def tpe: TypeRepr
    override final def inTpe: TypeRepr = tpe
    override final def outTpe: TypeRepr = tpe
    override protected final def convertTermInternal(term: Term)(using Quotes): Term = term
  }

  final case class AndThen(a: TermTransformer, b: TermTransformer) extends TermTransformer {
    override def inTpe: TypeRepr = a.inTpe
    override def outTpe: TypeRepr = b.outTpe
    override protected def convertTermInternal(term: Term)(using Quotes): Term = b.convertTerm(a.convertTerm(term))
  }

  trait Defer extends TermTransformer {
    protected def defer: TermTransformer

    override final lazy val inTpe: TypeRepr = defer.inTpe
    override final lazy val outTpe: TypeRepr = defer.outTpe
    override protected final def convertTermInternal(term: Term)(using Quotes): Term = defer.convertTermInternal(term)
  }

  trait DeferToParam extends Defer {
    val param: Function.Param
    override protected final def defer: TermTransformer = param
  }

}
