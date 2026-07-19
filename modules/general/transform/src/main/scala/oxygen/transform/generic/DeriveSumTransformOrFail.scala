package oxygen.transform.generic

import oxygen.meta.*
import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.transform.*
import scala.quoted.*

final case class DeriveSumTransformOrFail[From: Type, To: Type](
    fromGeneric: SumGeneric.FlatGeneric[From],
    toGeneric: SumGeneric.FlatGeneric[To],
)(using quotes: Quotes) {

  private val toCaseMap: Map[String, toGeneric.Case[? <: To]] =
    toGeneric.cases.map { c => (c.name, c) }.toMap

  private final case class CaseTransform[F <: From, T <: To](
      fromCase: fromGeneric.Case[F],
      toCase: toGeneric.Case[T],
      transformInstance: Expr[TransformOrFail[F, T]],
  ) {

    val valDef: ValDef = ValDef.newVal(s"transform_${fromCase.name}_${toCase.name}", ValDef.ValType.LazyVal)(transformInstance.toTerm)
    val cachedTransform: Expr[TransformOrFail[F, T]] = valDef.valRef.asExprOf[TransformOrFail[F, T]]

    given fType: Type[F] = fromCase.tpe
    given tType: Type[T] = toCase.tpe

    def matchBuilder: MatchBuilder[From, Either[TransformError, To]] =
      fromCase.caseExtractor.withRHS { from => '{ $cachedTransform.transformOrFail($from) } }

  }
  private object CaseTransform {

    def from[F <: From](fromCase: fromGeneric.Case[F]): CaseTransform[F, ?] = {
      type T <: To
      val _toCase: Option[toGeneric.Case[?]] =
        toCaseMap.get(fromCase.name)

      val toCase: toGeneric.Case[T] =
        _toCase
          .getOrElse {
            report.errorAndAbort(
              s"""Unable to derive Transform[From = ${fromGeneric.typeRepr.showAnsiCode}, To = ${toGeneric.typeRepr.showAnsiCode}]
              |
              |Unable to find matching case named ${fromCase.name} in ${toGeneric.typeRepr.showAnsiCode} which exists in ${fromGeneric.typeRepr.showAnsiCode}
              |""".stripMargin,
              fromCase.pos,
            )
          }
          .typedAs[T]

      given Type[F] = fromCase.tpe
      given Type[T] = toCase.tpe

      val rawTransformExpr: Expr[TransformOrFail[F, T]] =
        Implicits.searchOption[TransformOrFail[F, T]].getOrElse { DeriveProductTransformOrFail(fromCase.generic, toCase.generic).derive }

      val transformExpr: Expr[TransformOrFail[F, T]] =
        '{ $rawTransformExpr.atSubType(${ Expr(toCase.name) }) }

      CaseTransform(fromCase, toCase, transformExpr)
    }

  }

  private val transforms: ArraySeq[CaseTransform[?, ?]] =
    fromGeneric.cases.map(CaseTransform.from(_))

  private val rawTransformExpr: Expr[TransformOrFail[From, To]] =
    '{
      new TransformOrFail[From, To] {
        override def transformOrFail(from: From): Either[TransformError, To] = ${ MatchBuilder.merge(transforms.map(_.matchBuilder)).matchOn('from) }
      }
    }

  val derive: Expr[TransformOrFail[From, To]] =
    Block.companion
      .apply(
        transforms.map(_.valDef).toList,
        rawTransformExpr.toTerm,
      )
      .asExprOf[TransformOrFail[From, To]]

}
