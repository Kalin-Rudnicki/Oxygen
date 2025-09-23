package oxygen.transform.generic

import oxygen.meta.K0.*
import oxygen.quoted.*
import oxygen.transform.*
import scala.quoted.*

object TransformMacros {

  def derivedImpl[From: Type, To: Type](using Quotes): Expr[Transform[From, To]] = {
    val fromTypeRepr: TypeRepr = TypeRepr.of[From]
    val toTypeRepr: TypeRepr = TypeRepr.of[To]

    (fromTypeRepr.typeType.required, toTypeRepr.typeType.required) match {
      case (_: TypeType.Case, _: TypeType.Case)     => DeriveProductTransform(ProductGeneric.of[From], ProductGeneric.of[To]).derive
      case (_: TypeType.Sealed, _: TypeType.Sealed) => DeriveSumTransform(SumGeneric.FlatGeneric.of[From], SumGeneric.FlatGeneric.of[To]).derive
      case (fromType, toType)                       =>
        report.errorAndAbort(
          s"""Unable to derive Transform[${fromTypeRepr.showAnsiCode}, ${toTypeRepr.showAnsiCode}]
             |
             |Types are not the same structure of
             |${fromTypeRepr.showAnsiCode} : ${fromType.baseType}
             |${toTypeRepr.showAnsiCode} : ${toType.baseType}
             |""".stripMargin,
        )
    }
  }

}
