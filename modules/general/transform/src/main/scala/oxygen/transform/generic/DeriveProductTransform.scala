package oxygen.transform.generic

import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.transform.*
import scala.quoted.*

final case class DeriveProductTransform[From: Type, To: Type](
    fromGeneric: ProductGeneric[From],
    toGeneric: ProductGeneric[To],
)(using quotes: Quotes) {

  private val fromFieldMap: Map[String, fromGeneric.Field[?]] =
    fromGeneric.fields.map { f => (f.name, f) }.toMap

  private final case class FieldTransform[F, T](
      fromField: fromGeneric.Field[F],
      toField: toGeneric.Field[T],
      transformInstance: Expr[Transform[F, T]],
  ) {

    val valDef: ValDef = ValDef.newVal(s"transform_${fromField.name}_${toField.name}", ValDef.ValType.LazyVal)(transformInstance.toTerm)
    val cachedTransform: Expr[Transform[F, T]] = valDef.valRef.asExprOf[Transform[F, T]]

    given fType: Type[F] = fromField.tpe
    given tType: Type[T] = toField.tpe

    def convert(in: Expr[From]): Expr[T] =
      '{ $cachedTransform.transform(${ fromField.fromParent(in) }) }

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

      val transformExpr: Expr[Transform[F, T]] =
        Implicits.searchOption[Transform[F, T]].getOrElse {
          report.errorAndAbort(
            s"""Unable to derive Transform[From = ${fromGeneric.typeRepr.showAnsiCode}, To = ${toGeneric.typeRepr.showAnsiCode}]
               |
               |Unable to find field mapping instance for field ${toField.name}: Transform[From = ${fromField.typeRepr.showAnsiCode}, To = ${toField.typeRepr.showAnsiCode}]
               |""".stripMargin,
            toField.pos,
          )
        }

      FieldTransform(fromField, toField, transformExpr)
    }

  }

  private val transforms: ArraySeq[FieldTransform[?, ?]] =
    toGeneric.fields.map(FieldTransform.from(_))

  private val rawTransformExpr: Expr[Transform[From, To]] =
    '{
      new Transform[From, To] {
        override def transform(from: From): To = ${ toGeneric.instantiate.fieldsToInstance(transforms.map(_.convert('from))) }
      }
    }

  val derive: Expr[Transform[From, To]] =
    Block.companion
      .apply(
        transforms.map(_.valDef).toList,
        rawTransformExpr.toTerm,
      )
      .asExprOf[Transform[From, To]]

}
