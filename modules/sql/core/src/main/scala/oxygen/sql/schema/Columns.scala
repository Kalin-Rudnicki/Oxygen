package oxygen.sql.schema

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.sql.generic.typeclass.*
import scala.quoted.*

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
object Columns extends K0.Derivable[Columns] {

  override protected def productDeriver[A](using Quotes, Type[Columns], Type[A], K0.ProductGeneric[A], K0.Derivable[Columns]): K0.Derivable.ProductDeriver[Columns, A] =
    K0.Derivable.ProductDeriver.withInstances[Columns, A] { DeriveProductColumns(_) }

  override protected def sumDeriver[A](using Quotes, Type[Columns], Type[A], K0.SumGeneric[A], K0.Derivable[Columns]): K0.Derivable.SumDeriver[Columns, A] =
    K0.Derivable.SumDeriver.notSupported

  override inline def derived[A]: Columns[A] = ${ derivedImpl[A] }

}
