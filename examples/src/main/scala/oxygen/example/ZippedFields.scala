package oxygen.example

import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final class ZippedFields[From, To](
    val fromGeneric: ProductGeneric[From],
    val toGeneric: ProductGeneric[To],
)(
    val zipped: Contiguous[(fromGeneric.Field[?], toGeneric.Field[?])],
) {

  def map2[Out](f: [_from, _to] => (Type[_from], Type[_to]) ?=> (fromGeneric.Field[_from], toGeneric.Field[_to]) => Out): Contiguous[Out] =
    zipped.map { (fromField0, toField0) =>
      type _From
      type _To
      val fromField: fromGeneric.Field[_From] = fromField0.typedAs[_From]
      val toField: toGeneric.Field[_To] = toField0.typedAs[_To]

      f[_From, _To](using fromField.tpe, toField.tpe)(fromField, toField)
    }

}
object ZippedFields {

  def of[From: Type, To: Type](using Quotes): ZippedFields[From, To] = {
    val fromGeneric: ProductGeneric[From] = ProductGeneric.of[From]
    val toGeneric: ProductGeneric[To] = ProductGeneric.of[To]

    val fromFieldMap: Map[String, fromGeneric.Field[?]] =
      fromGeneric.fields.map { fromField => (fromField.name, fromField) }.toMap

    ZippedFields[From, To](fromGeneric, toGeneric)(
      toGeneric.fields.map { toField =>
        (
          fromFieldMap.getOrElse(
            toField.name,
            report.errorAndAbort(
              s"""Wont be able to populate ${toField.name} in ${TypeRepr.of[To].showAnsiCode},
                 |because ${TypeRepr.of[From].showAnsiCode} does not have have a field with that name
                 |""".stripMargin,
              toField.pos,
            ),
          ),
          toField,
        )
      },
    )
  }

}
