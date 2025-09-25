package oxygen.sql.query.dsl

import oxygen.sql.schema.*

object Q {

  def const[I](i: I): I = macroOnly

  object input {
    def apply[I]: T.Input[I] = macroOnly
    // TODO (KR) : def option[A]: T.OptionalInput[I] = macroOnly
    def const[I](i: I): T.ConstInput[I] = macroOnly
  }

  def insert[A](using t: TableRepr[A]): T.Insert[A] = macroOnly
  def select[A](using t: TableRepr[A]): T.Select[A] = macroOnly
  def update[A](using t: TableRepr[A]): T.Update[A] = macroOnly
  def delete[A](using t: TableRepr[A]): T.Delete[A] = macroOnly

  def where: T.Partial.Where = macroOnly
  def join[A](using t: TableRepr[A]): T.Partial.Join[A] = macroOnly
  def leftJoin[A](using t: TableRepr[A]): T.Partial.LeftJoin[A] = macroOnly

  // TODO (KR) : support auto generated natural joins
  // def natural: Boolean = macroOnly

  def limit(lim: Int): T.Limit = macroOnly
  def offset(off: Int): T.Offset = macroOnly

  def orderBy(parts: T.Partial.OrderByPart*): T.OrderBy = macroOnly

  extension [A](self: A) {
    def tablePK(using ev: TableRepr[A]): ev.PrimaryKeyT = ev.pk.get(self)
    def tableNPK(using ev: TableRepr[A]): ev.NonPrimaryKeyT = ev.npk.get(self)
  }

  extension [A](self: A)
    def :=(value: A): T.Partial.SetValue = macroOnly
    def asc: T.Partial.OrderByPart = macroOnly
    def desc: T.Partial.OrderByPart = macroOnly

}
