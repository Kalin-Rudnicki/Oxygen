package oxygen.transform.generic

import oxygen.meta.k0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.transform.*
import scala.quoted.*

final case class DeriveProductTransformOrFail[From: Type, To: Type](
    fromGeneric: ProductGeneric[From],
    toGeneric: ProductGeneric[To],
)(using quotes: Quotes) {

  private val fromFieldMap: Map[String, fromGeneric.Field[?]] =
    fromGeneric.fields.map { f => (f.name, f) }.toMap

  private final case class FieldTransform[F, T](
      fromField: fromGeneric.Field[F],
      toField: toGeneric.Field[T],
      transformInstance: Expr[TransformOrFail[F, T]],
  ) {

    type _F = F
    type _T = T

    val valDef: ValDef = ValDef.newVal(s"transform_${fromField.name}_${toField.name}", ValDef.ValType.LazyVal)(transformInstance.toTerm)
    val cachedTransform: Expr[TransformOrFail[F, T]] = valDef.valRef.asExprOf[TransformOrFail[F, T]]

    given fType: Type[F] = fromField.tpe
    given tType: Type[T] = toField.tpe

    def convert(in: Expr[From]): Expr[Either[TransformError, T]] =
      '{ $cachedTransform.transformOrFail(${ fromField.fromParent(in) }) }

  }
  private object FieldTransform {

    def from[T](toField: toGeneric.Field[T]): FieldTransform[?, T] = {
      type F
      val _fromField: Option[fromGeneric.Field[?]] =
        fromFieldMap.get(toField.name)

      val fromField: fromGeneric.Field[F] =
        _fromField
          .getOrElse {
            report.errorAndAbort(
              s"""Unable to derive Transform[From = ${fromGeneric.typeRepr.showAnsiCode}, To = ${toGeneric.typeRepr.showAnsiCode}]
              |
              |Unable to find matching field named ${toField.name} in ${fromGeneric.typeRepr.showAnsiCode} which exists in ${toGeneric.typeRepr.showAnsiCode}
              |""".stripMargin,
              toField.pos,
            )
          }
          .typedAs[F]

      given Type[F] = fromField.tpe
      given Type[T] = toField.tpe

      val rawTransformExpr: Expr[TransformOrFail[F, T]] =
        Implicits.searchOption[TransformOrFail[F, T]].getOrElse {
          report.errorAndAbort(
            s"""Unable to derive Transform[From = ${fromGeneric.typeRepr.showAnsiCode}, To = ${toGeneric.typeRepr.showAnsiCode}]
               |
               |Unable to find field mapping instance for field ${toField.name}: Transform[From = ${fromField.typeRepr.showAnsiCode}, To = ${toField.typeRepr.showAnsiCode}]
               |""".stripMargin,
            toField.pos,
          )
        }

      val transformExpr: Expr[TransformOrFail[F, T]] =
        '{ $rawTransformExpr.atField(${ Expr(toField.name) }) }

      FieldTransform(fromField, toField, transformExpr)
    }

  }

  private val transforms: ArraySeq[FieldTransform[?, ?]] =
    toGeneric.fields.map(FieldTransform.from(_))

  private def buildFlatMapExpr(
      fromExpr: Expr[From],
      queue: List[FieldTransform[?, ?]],
      rStack: List[Expr[?]],
  )(using Quotes): Expr[Either[TransformError, To]] =
    queue match {
      case head :: Nil =>
        type T = head._T
        given Type[T] = head.tType
        '{ ${ head.convert(fromExpr) }.map { value => ${ toGeneric.fieldsToInstance(('value :: rStack).reverse) } } }
      case head :: tail =>
        type T = head._T
        given Type[T] = head.tType
        '{ ${ head.convert(fromExpr) }.flatMap { value => ${ buildFlatMapExpr(fromExpr, tail, 'value :: rStack) } } }
      case Nil if rStack.isEmpty => '{ ${ toGeneric.fieldsToInstance(Nil) }.asRight }
      case Nil                   => report.errorAndAbort("Internal Defect - queue is empty and stack is not")
    }

  private val rawTransformExpr: Expr[TransformOrFail[From, To]] =
    '{
      new TransformOrFail[From, To] {
        override def transformOrFail(from: From): Either[TransformError, To] =
          ${ buildFlatMapExpr('from, transforms.toList, Nil) }
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
