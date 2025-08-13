package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.error.*
import oxygen.zio.instances.given
import scala.reflect.ClassTag
import zio.*
import zio.stream.*

object QueryResult {

  final class Returning[E, A] private[sql] (
      ctx: QueryContext,
      effect: ZStream[Database, E, A],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): Returning[Nothing, A] =
      Returning(ctx, effect.orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): Returning[Nothing, A] =
      Returning(ctx, effect.orDieWith(f))

    def mapError[E2](f: E => E2): Returning[E2, A] =
      Returning(ctx, effect.mapError(f))

    // =====| Results |=====

    // TODO (KR) : it might potentially be worth optimizing for `single/option` queries,
    //           : and avoiding the overhead of creating a whole stream.
    //           : It should now be possible using `toSink`

    def singleOrElse[E2 >: E](onEmpty: => E2, onMany: => E2): ZIO[Database, E2, A] =
      this.toSink(Sinks.option(onMany)).someOrFail(onEmpty)

    def single(using ev: QueryError <:< E): ZIO[Database, E, A] =
      this.singleOrElse(
        ev(QueryError(ctx, QueryError.InvalidResultSetSize(QueryError.InvalidResultSetSize.ExpectedSize.Single, "0"))),
        ev(QueryError(ctx, QueryError.InvalidResultSetSize(QueryError.InvalidResultSetSize.ExpectedSize.Single, "many"))),
      )

    def optionOrElse[E2 >: E](onMany: => E2): ZIO[Database, E2, Option[A]] =
      this.toSink(Sinks.option(onMany))

    def option(using ev: QueryError <:< E): ZIO[Database, E, Option[A]] =
      this.optionOrElse(
        ev(QueryError(ctx, QueryError.InvalidResultSetSize(QueryError.InvalidResultSetSize.ExpectedSize.Single, "many"))),
      )

    def chunk: ZIO[Database, E, Chunk[A]] = this.to[Chunk]
    def arraySeq(using ClassTag[A]): ZIO[Database, E, ArraySeq[A]] = this.toSink(Sinks.arraySeq)

    // =====| Root Result Functions |=====

    def toSink[E1 >: E, B](sink: ZSink[Any, E1, A, Nothing, B]): ZIO[Database, E1, B] =
      this.effect.run(sink) @@ ctx.metrics.track(QueryContext.ExecutionType.Query)

    def toSinkT[E1 >: E, T[_]](sink: ZSink[Any, E1, A, Nothing, T[A]]): ZIO[Database, E1, T[A]] = this.toSink(sink)
    def to[S[_]: SeqOps as ops]: ZIO[Database, E, S[A]] = this.toSink(Sinks.seq[S, A])

    // TODO (KR) : support stream metrics
    def stream: ZStream[Database, E, A] =
      effect

  }

  final class Update[E] private[sql] (
      ctx: QueryContext,
      batchSize: Option[Int],
      effect: ZIO[Database, E, Int],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): Update[Nothing] =
      Update(ctx, batchSize, effect.orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): Update[Nothing] =
      Update(ctx, batchSize, effect.orDieWith(f))

    def mapError[E2](f: E => E2): Update[E2] =
      Update(ctx, batchSize, effect.mapError(f))

    // =====| Results |=====

    def updated: ZIO[Database, E, Int] =
      effect @@ ctx.metrics.track(batchSize.fold(QueryContext.ExecutionType.Update)(QueryContext.ExecutionType.AggregatedBatchUpdate(_)))

    def unit: ZIO[Database, E, Unit] =
      updated.unit

    // TODO (KR) : validate number of updated rows

  }

  final class BatchUpdate[E] private[sql] (
      ctx: QueryContext,
      batchSize: Int,
      effect: ZIO[Database, E, ArraySeq[Int]],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): BatchUpdate[Nothing] =
      BatchUpdate(ctx, batchSize, effect.orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): BatchUpdate[Nothing] =
      BatchUpdate(ctx, batchSize, effect.orDieWith(f))

    def mapError[E2](f: E => E2): BatchUpdate[E2] =
      BatchUpdate(ctx, batchSize, effect.mapError(f))

    // =====| Results |=====

    def updated: ZIO[Database, E, ArraySeq[Int]] =
      effect @@ ctx.metrics.track(QueryContext.ExecutionType.JdbcBatchUpdate(batchSize))

    def totalUpdated: ZIO[Database, E, Int] =
      updated.map(_.sum)

    def unit: ZIO[Database, E, Unit] =
      updated.unit

    // TODO (KR) : validate number of updated rows

  }

}
