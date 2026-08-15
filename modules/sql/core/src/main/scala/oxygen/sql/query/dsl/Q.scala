package oxygen.sql.query.dsl

import oxygen.sql.query.QueryO
import oxygen.sql.schema.*

object Q {

  def const[I](i: I): I = macroOnly

  object input {

    def apply[I]: T.Input[I] = macroOnly

    def optional[I]: T.OptionalInput[I] = macroOnly

    def const[I](i: I): T.ConstInput[I] = macroOnly

    /** A `Seq` input bound as a single `?` param. Use `arr.contains(col)` for `col = ANY(?)`. */
    def array[I]: T.ArrayInput[I] = macroOnly

    /** A `Set` input bound as a single `?` param. Use `arr.contains(col)` for `col = ANY(?)`. */
    def set[I]: T.SetInput[I] = macroOnly

  }

  object select {

    def apply[A](using t: TableRepr[A]): T.Select[A] = macroOnly

    def subQuery[A](subQueryTableName: String)(q: QueryO[A]): T.SelectSubQuery[A] = macroOnly

    /**
      * Use an array/collection input as a `UNNEST(?)` table source (one row per element),
      * aliased so it can be JOINed/filtered/selected against.
      *
      * {{{
      *   for {
      *     ids <- input.array[UUID]
      *     id  <- select.unnest(ids)
      *     n   <- join[Note] if n.personId == id
      *   } yield n
      *   // SELECT ... FROM UNNEST(?::uuid[]) id JOIN note n ON n.person_id = id
      * }}}
      */
    def unnest[A](arr: Iterable[A])(using r: RowRepr[A]): T.SelectUnnest[A] = macroOnly

  }

  object insert {

    def apply[A](using t: TableRepr[A]): T.Insert[A] = macroOnly

    def fromSelect[A](using t: TableRepr[A]): T.InsertFromSelect[A] = macroOnly

  }

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

  object count {
    def apply[A](toCount: A): Long = macroOnly
    def * : Long = macroOnly
    def _1: Long = macroOnly
  }

  extension [A](self: A) {
    def tablePK(using ev: TableRepr[A]): ev.PrimaryKeyT = ev.pk.get(self)
    def tableNPK(using ev: TableRepr[A]): ev.NonPrimaryKeyT = ev.npk.get(self)
  }

  extension [A](self: A)
    def :=(value: A): T.Partial.SetValue = macroOnly
    def asc: T.Partial.OrderByPart = macroOnly
    def desc: T.Partial.OrderByPart = macroOnly
    def <=>(value: A): Double = macroOnly // Cosine distance (used for vector similarity)
    def <#>(value: A): Double = macroOnly // Negative inner product (used for vector similarity)
    def <->(value: A): Double = macroOnly // Euclidean (L2) distance
    def <+>(value: A): Double = macroOnly // L1 (Manhattan) distance
    def @>(value: A): Boolean = macroOnly // is ancestor of (ltree)
    def <@(value: A): Boolean = macroOnly // is descendant of (ltree)

  def mkSqlString(strings: String*): String = macroOnly

}
