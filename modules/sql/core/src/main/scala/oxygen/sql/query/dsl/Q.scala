package oxygen.sql.query.dsl

import oxygen.predef.core.*
import oxygen.sql.query.QueryO
import oxygen.sql.schema.*

object Q {

  def const[I](i: I): I = macroOnly

  object input {

    def apply[I]: T.Input[I] = macroOnly

    def optional[I]: T.OptionalInput[I] = macroOnly

    def const[I](i: I): T.ConstInput[I] = macroOnly

  }

  object select {

    def apply[A](using t: TableRepr[A]): T.Select[A] = macroOnly

    def subQuery[A](subQueryTableName: String)(q: QueryO[A]): T.SelectSubQuery[A] = macroOnly

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

  object agg {

    object types { // FIX-PRE-MERGE (KR) : dont leave garbage

      /////// General ///////////////////////////////////////////////////////////////

      type C[T0] = [S[_]] =>> T0

      type AS[T0[_[_]]] = [S[_]] =>> S[T0[S]]

      // format: off
      type S1[T0]                 = [S[_]] =>> S[ T0                        ]
      type S2[T0, T1]             = [S[_]] =>> S[(T0,  S[T1]               )]
      type S3[T0, T1, T2]         = [S[_]] =>> S[(T0, S2[T1, T2][S]        )]
      type S4[T0, T1, T2, T3]     = [S[_]] =>> S[(T0, S3[T1, T2, T3][S]    )]
      type S5[T0, T1, T2, T3, T4] = [S[_]] =>> S[(T0, S4[T1, T2, T3, T4][S])]
      // format: on

      /////// App1 ///////////////////////////////////////////////////////////////

      // format: off
      type AppC[T0] =   [T1      ] =>> [S[_]] =>>   (T0, T1      )
      type AppK[T0] =   [T1[_[_]]] =>> [S[_]] =>>   (T0, T1[S]   )
      type AppS[T0] =   [T1      ] =>> [S[_]] =>>   (T0, S[T1]   )
      type AppZ[T0] =   [T1[_[_]]] =>> [S[_]] =>>   (T0, S[T1[S]])
      // format: on

      /////// App2 ///////////////////////////////////////////////////////////////

      // format: off
      type AppCC[T0] =   [T1      , T2      ] =>> [S[_]] =>>   (T0, T1      , T2      )
      type AppCK[T0] =   [T1      , T2[_[_]]] =>> [S[_]] =>>   (T0, T1      , T2[S]   )
      type AppCS[T0] =   [T1      , T2      ] =>> [S[_]] =>>   (T0, T1      , S[T2]   )
      type AppCZ[T0] =   [T1      , T2[_[_]]] =>> [S[_]] =>>   (T0, T1      , S[T2[S]])

      type AppKC[T0] =   [T1[_[_]], T2      ] =>> [S[_]] =>>   (T0, T1[S]   , T2      )
      type AppKK[T0] =   [T1[_[_]], T2[_[_]]] =>> [S[_]] =>>   (T0, T1[S]   , T2[S]   )
      type AppKS[T0] =   [T1[_[_]], T2      ] =>> [S[_]] =>>   (T0, T1[S]   , S[T2]   )
      type AppKZ[T0] =   [T1[_[_]], T2[_[_]]] =>> [S[_]] =>>   (T0, T1[S]   , S[T2[S]])

      type AppSC[T0] =   [T1      , T2      ] =>> [S[_]] =>>   (T0, S[T1]   , T2      )
      type AppSK[T0] =   [T1      , T2[_[_]]] =>> [S[_]] =>>   (T0, S[T1]   , T2[S]   )
      type AppSS[T0] =   [T1      , T2      ] =>> [S[_]] =>>   (T0, S[T1]   , S[T2]   )
      type AppSZ[T0] =   [T1      , T2[_[_]]] =>> [S[_]] =>>   (T0, S[T1]   , S[T2[S]])

      type AppZC[T0] =   [T1[_[_]], T2      ] =>> [S[_]] =>>   (T0, S[T1[S]], T2      )
      type AppZK[T0] =   [T1[_[_]], T2[_[_]]] =>> [S[_]] =>>   (T0, S[T1[S]], T2[S]   )
      type AppZS[T0] =   [T1[_[_]], T2      ] =>> [S[_]] =>>   (T0, S[T1[S]], S[T2]   )
      type AppZZ[T0] =   [T1[_[_]], T2[_[_]]] =>> [S[_]] =>>   (T0, S[T1[S]], S[T2[S]])
      // format: on

      /////// App3 ///////////////////////////////////////////////////////////////

      // format: off
      type AppCCC[T0] =   [T1      , T2      , T3      ] =>> [S[_]] =>>   (T0, T1      , T2      , T3      )
      type AppKKK[T0] =   [T1[_[_]], T2[_[_]], T3[_[_]]] =>> [S[_]] =>>   (T0, T1[S]   , T2[S]   , T3[S]   )
      type AppSSS[T0] =   [T1      , T2      , T3      ] =>> [S[_]] =>>   (T0, S[T1]   , S[T2]   , S[T3]   )
      type AppZZZ[T0] =   [T1[_[_]], T2[_[_]], T3[_[_]]] =>> [S[_]] =>>   (T0, S[T1[S]], S[T2[S]], S[T3[S]])
      // format: on

      /////// App4 ///////////////////////////////////////////////////////////////

      // format: off
      type AppCCCC[T0] =   [T1      , T2      , T3      , T4      ] =>> [S[_]] =>>   (T0, T1      , T2      , T3      , T4      )
      type AppKKKK[T0] =   [T1[_[_]], T2[_[_]], T3[_[_]], T4[_[_]]] =>> [S[_]] =>>   (T0, T1[S]   , T2[S]   , T3[S]   , T4[S]   )
      type AppSSSS[T0] =   [T1      , T2      , T3      , T4      ] =>> [S[_]] =>>   (T0, S[T1]   , S[T2]   , S[T3]   , S[T4]   )
      type AppZZZZ[T0] =   [T1[_[_]], T2[_[_]], T3[_[_]], T4[_[_]]] =>> [S[_]] =>>   (T0, S[T1[S]], S[T2[S]], S[T3[S]], S[T4[S]])
      // format: on

      /////// App5 ///////////////////////////////////////////////////////////////

      // format: off
      type AppCCCCC[T0] =   [T1      , T2      , T3      , T4      , T5      ] =>> [S[_]] =>>   (T0, T1      , T2      , T3      , T4      , T5      )
      type AppKKKKK[T0] =   [T1[_[_]], T2[_[_]], T3[_[_]], T4[_[_]], T5[_[_]]] =>> [S[_]] =>>   (T0, T1[S]   , T2[S]   , T3[S]   , T4[S]   , T5[S]   )
      type AppSSSSS[T0] =   [T1      , T2      , T3      , T4      , T5      ] =>> [S[_]] =>>   (T0, S[T1]   , S[T2]   , S[T3]   , S[T4]   , S[T5]   )
      type AppZZZZZ[T0] =   [T1[_[_]], T2[_[_]], T3[_[_]], T4[_[_]], T5[_[_]]] =>> [S[_]] =>>   (T0, S[T1[S]], S[T2[S]], S[T3[S]], S[T4[S]], S[T5[S]])
      // format: on

    }

    final class AppliedAgg[F[_]] {
      def apply[A](f: QueryO[A]): F[A] = macroOnly
    }

    def required: AppliedAgg[[A] =>> A] = macroOnly
    def optional: AppliedAgg[Option] = macroOnly
    def many[S[_]](using seqOps: SeqOps[S]): AppliedAgg[S] = macroOnly
    def manyNonEmpty: AppliedAgg[NonEmptyList] = macroOnly

  }

  extension [A](self: A) {
    def tablePK(using ev: TableRepr[A]): ev.PrimaryKeyT = ev.pk.get(self)
    def tableNPK(using ev: TableRepr[A]): ev.NonPrimaryKeyT = ev.npk.get(self)
  }

  extension [A](self: A)
    def :=(value: A): T.Partial.SetValue = macroOnly
    def asc: T.Partial.OrderByPart = macroOnly
    def desc: T.Partial.OrderByPart = macroOnly

  def mkSqlString(strings: String*): String = macroOnly

}
