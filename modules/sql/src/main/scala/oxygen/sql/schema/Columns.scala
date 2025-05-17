package oxygen.sql.schema

import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.generic.*

final case class Columns[A](columns: Contiguous[Column]) {

  val size: Int = columns.length

  extension (str: String) private def surroundIfMulti: String = if (size == 1) str else s"($str)"

  // Are these functions named super weird? yes.
  // Are they very descriptive? yes.
  // Parens are only ever added around multiple columns.
  val `?, ?, ?`: String = columns.map(_ => "?").mkString(", ")
  val `(?, ?, ?)`: String = `?, ?, ?`.surroundIfMulti
  val `a, b, c`: String = columns.map(_.name).mkString(", ")
  val `(a, b, c)`: String = `a, b, c`.surroundIfMulti
  def `ref.a, ref.b, ref.c`(ref: String): String = columns.map(c => s"$ref.${c.name}").mkString(", ")
  def `(ref.a, ref.b, ref.c)`(ref: String): String = `ref.a, ref.b, ref.c`(ref).surroundIfMulti

}
object Columns extends K0.Derivable.WithInstances[Columns] {

  override protected def internalDeriveProductI[Q <: Quotes, A](k0: K0[Q])(
      g: k0.ProductGeneric[A],
      i: k0.ValExpressions[Columns],
  )(using quotes: Q, aTpe: Type[A], tTpe: Type[Columns]): Expr[Columns[A]] =
    DeriveProductColumns[Q, A](k0)(g, i).makeColumns

  override protected def internalDeriveSumI[Q <: Quotes, A](k0: K0[Q])(
      g: k0.SumGeneric[A],
      i: k0.ValExpressions[Columns],
  )(using quotes: Q, aTpe: Type[A], tTpe: Type[Columns]): Expr[Columns[A]] =
    k0.meta.report.errorAndAbort("Not supported: Columns.derive for sum type")

}
