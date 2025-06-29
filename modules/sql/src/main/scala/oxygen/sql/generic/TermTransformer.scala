package oxygen.sql.generic

import oxygen.quoted.*
import scala.quoted.*

private[generic] trait TermTransformer {

  def inTpe: TypeRepr
  def outTpe: TypeRepr

  def convertTerm(term: Term)(using Quotes): Term

  final def getExpr[Out: Type](in: Expr[?])(using Quotes): Expr[Out] = {
    val fInTerm: Term = in.toTerm
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

    fOutTerm.asExprOf[Out]
  }

}
