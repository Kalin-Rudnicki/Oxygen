package oxygen.sql.query.dsl

import oxygen.predef.core.*
import oxygen.sql.query.{Query, QueryI, QueryIO, QueryO}

object T {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Inputs
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait InputLike

  final class Input[I] private extends InputLike {
    def flatMap(f: I => Query): QueryI[I] = macroOnly
    def flatMap[I2](f: I => QueryI[I2])(using zip: Zip[I, I2]): QueryI[zip.Out] = macroOnly
    def flatMap[O](f: I => QueryO[O]): QueryIO[I, O] = macroOnly
    def flatMap[I2, O](f: I => QueryIO[I2, O])(using zip: Zip[I, I2]): QueryIO[zip.Out, O] = macroOnly
  }

  final class OptionalInput[I] private extends InputLike {
    def flatMap(f: I => Query): QueryI[Option[I]] = macroOnly
    def flatMap[I2](f: I => QueryI[I2])(using zip: Zip[Option[I], I2]): QueryI[zip.Out] = macroOnly
    def flatMap[O](f: I => QueryO[O]): QueryIO[Option[I], O] = macroOnly
    def flatMap[I2, O](f: I => QueryIO[I2, O])(using zip: Zip[Option[I], I2]): QueryIO[zip.Out, O] = macroOnly
  }

  final class ConstInput[I] private extends InputLike {
    def flatMap(f: I => Query): Query = macroOnly
    def flatMap[I2](f: I => QueryI[I2]): QueryI[I2] = macroOnly
    def flatMap[O](f: I => QueryO[O]): QueryO[O] = macroOnly
    def flatMap[I2, O](f: I => QueryIO[I2, O]): QueryIO[I2, O] = macroOnly
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      CRUD
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class Insert[A] private {
    def map[B](f: (A, Partial.InsertValues[A]) => B)(using retTpe: RetTpe[B]): retTpe.QueryT = macroOnly
    def flatMap[B](f: (A, Partial.InsertValues[A]) => Ret[B])(using retTpe: RetTpe[B]): retTpe.QueryT = macroOnly
  }

  final class Select[A] private {
    def map[B](f: A => B): QueryO[B] = macroOnly
    def flatMap[B](f: A => Ret[B]): QueryO[B] = macroOnly
  }

  final class Update[A] private {
    def map[B](f: (A, Partial.UpdateSet[A]) => Unit)(using retTpe: RetTpe[B]): retTpe.QueryT = macroOnly
    def flatMap[B](f: (A, Partial.UpdateSet[A]) => Ret[B])(using retTpe: RetTpe[B]): retTpe.QueryT = macroOnly
  }

  final class Delete[A] private {
    def map[B](f: A => B)(using retTpe: RetTpe[B]): retTpe.QueryT = macroOnly
    def flatMap[B](f: A => Ret[B])(using retTpe: RetTpe[B]): retTpe.QueryT = macroOnly
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other Root
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class PlainLateral[A] private {
    def map[B](f: A => B): Ret[B] = macroOnly
    def flatMap[B](f: A => Ret[B]): Ret[B] = macroOnly
  }

  final class Join[A] private {

    def map[B](f: A => B): Ret[B] = macroOnly
    def flatMap[B](f: A => Ret[B]): Ret[B] = macroOnly

  }

  final class Where private {

    def map[B](f: Unit => B): Ret[B] = macroOnly
    def flatMap[B](f: Unit => Ret[B]): Ret[B] = macroOnly

  }

  final class InsertValues private {

    def map[B](f: Unit => B): Ret[B] = macroOnly
    def flatMap[B](f: Unit => Ret[B]): Ret[B] = macroOnly

  }

  final class UpdateSet private {

    def map[B](f: Unit => B): Ret[B] = macroOnly
    def flatMap[B](f: Unit => Ret[B]): Ret[B] = macroOnly

  }

  final class Limit private {

    def map[B](f: Unit => B): Ret[B] = macroOnly
    def flatMap[B](f: Unit => Ret[B]): Ret[B] = macroOnly

  }

  final class Offset private {

    def map[B](f: Unit => B): Ret[B] = macroOnly
    def flatMap[B](f: Unit => Ret[B]): Ret[B] = macroOnly

  }

  final class OrderBy private {

    def map[B](f: Unit => B): Ret[B] = macroOnly
    def flatMap[B](f: Unit => Ret[B]): Ret[B] = macroOnly

  }

  object Partial {

    final class OrderByPart private {}

    final class SetValue private {}

    final class Where private {
      def withFilter(f: Unit => Boolean): T.Where = macroOnly
    }

    sealed trait JoinLike

    final class Join[A] private extends JoinLike {
      def withFilter(f: A => Boolean): T.Join[A] = macroOnly
    }

    final class LeftJoin[A] private extends JoinLike {
      def withFilter(f: A => Boolean): T.Join[Option[A]] = macroOnly
    }

    final class InsertValues[A] private {
      def apply(v: A): T.InsertValues = macroOnly
    }

    final class UpdateSet[A] private {
      def apply(set0: A => SetValue, setN: (A => SetValue)*): T.UpdateSet = macroOnly
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Intermediate
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : Adding more specific types here could make it easier for the user to tell what will actually be parsable.
  //           : Not having the type safety here is technically ok, as correctness is enforced by the macro.
  //           : That being said, it would be better if this was stricter.

  final class Ret[A] private {}

  trait RetTpe[O] {
    type QueryT
  }
  object RetTpe extends RetTpeLowPriority {
    type Aux[O, _QueryT] = RetTpe[O] { type QueryT = _QueryT }

    given RetTpe.Aux[Unit, Query] =
      new RetTpe[Unit] { override type QueryT = Query }

  }

  trait RetTpeLowPriority {

    given [O] => RetTpe.Aux[O, QueryO[O]] =
      new RetTpe[O] { override type QueryT = QueryO[O] }

  }

}
