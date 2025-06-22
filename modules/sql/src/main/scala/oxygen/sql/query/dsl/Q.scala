package oxygen.sql.query.dsl

import oxygen.core.collection.Contiguous
import oxygen.sql.query.QueryIO
import oxygen.sql.schema.*

object Q {

  object input {
    def apply[I]: T.Input[I] = macroOnly
    // TODO (KR) : def option[A]: T.OptionalInput[I] = macroOnly
    def const[I](i: I): T.ConstInput[I] = macroOnly
  }

  def insert[A](using t: TableRepr[A, ?]): T.Insert[A] = macroOnly
  def select[A](using t: TableRepr[A, ?]): T.Select[A] = macroOnly
  def update[A](using t: TableRepr[A, ?]): T.Update[A] = macroOnly
  def delete[A](using t: TableRepr[A, ?]): T.Delete[A] = macroOnly

  def where: T.Partial.Where = macroOnly
  def join[A](using t: TableRepr[A, ?]): T.Partial.Join[A] = macroOnly
  def leftJoin[A](using t: TableRepr[A, ?]): T.Partial.LeftJoin[A] = macroOnly

  extension [A](self: A)
    def :=(value: A): T.Partial.SetValue =
      macroOnly

  extension [I, O](self: QueryIO[I, O])
    def subQueryAll(i: I): Contiguous[O] =
      macroOnly

}
