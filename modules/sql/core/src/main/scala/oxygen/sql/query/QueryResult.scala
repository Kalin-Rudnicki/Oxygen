package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.error.*
import oxygen.zio.SparseStreamAggregator
import oxygen.zio.instances.given
import scala.reflect.ClassTag
import zio.*
import zio.stream.*

object QueryResult {

  final class Returning[E, A] private[sql] (
      ctx: QueryContext,
      effect: Returning.Config => ZStream[Database, E, A],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): Returning[Nothing, A] =
      Returning(ctx, effect(_).orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): Returning[Nothing, A] =
      Returning(ctx, effect(_).orDieWith(f))

    private[sql] def map[A2](f: A => A2): Returning[E, A2] =
      Returning(ctx, effect(_).map(f))

    private[sql] def mapOrFail[A2](f: A => Either[String, A2])(using ev: QueryError <:< E): Returning[E, A2] =
      Returning(
        ctx,
        effect(_).flatMap { a =>
          f(a) match {
            case Right(value) => ZStream.succeed(value)
            case Left(error)  => ZStream.fail(ev(QueryError(ctx, QueryError.UnableToDecodeRow.DelayedMapOrFail(a, error))))
          }
        },
      )

    def mapError[E2](f: E => E2): Returning[E2, A] =
      Returning(ctx, effect(_).mapError(f))

    // TODO (KR) : This could technically accept a `ZPipeline[Database, ...]` but I am limiting it for now
    def >>>[E2 >: E, B](pipeline: ZPipeline[Any, E2, A, B]): Returning[E2, B] =
      Returning(ctx, cfg => effect(cfg) >>> pipeline)

    def >>>[B](agg: SparseStreamAggregator[A, B]): Returning[E, B] =
      this >>> agg.toPipeline

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
      this.effect(Returning.Config.default).run(sink) @@ ctx.metrics.track(QueryContext.ExecutionType.Query)

    def toSinkT[E1 >: E, T[_]](sink: ZSink[Any, E1, A, Nothing, T[A]]): ZIO[Database, E1, T[A]] = this.toSink(sink)
    def to[S[_]: SeqOps as ops]: ZIO[Database, E, S[A]] = this.toSink(Sinks.seq[S, A])

    // TODO (KR) : support stream metrics
    def stream: ZStream[Database, E, A] = effect(Returning.Config.default)
    def streamWithFetchSize(fetchSize: Int): ZStream[Database, E, A] = effect(Returning.Config(fetchSize.some))

  }
  object Returning {

    // TODO (KR) : some of the batch stuff probably belongs in here
    final case class Config(fetchSize: Option[Int])
    object Config {
      val default: Config = Config(None)
    }

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

  final class OptimizedBatchUpdate[E] private[sql] (
      ctx: QueryContext,
      batchSize: Int,
      effect: ZIO[Database, E, Long],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): OptimizedBatchUpdate[Nothing] =
      OptimizedBatchUpdate(ctx, batchSize, effect.orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): OptimizedBatchUpdate[Nothing] =
      OptimizedBatchUpdate(ctx, batchSize, effect.orDieWith(f))

    def mapError[E2](f: E => E2): OptimizedBatchUpdate[E2] =
      OptimizedBatchUpdate(ctx, batchSize, effect.mapError(f))

    // =====| Results |=====

    def updated: ZIO[Database, E, Long] =
      effect @@ ctx.metrics.track(QueryContext.ExecutionType.AggregatedBatchUpdate(batchSize))

    def unit: ZIO[Database, E, Unit] =
      updated.unit

  }

  final class OptimizedBatchBatchUpdate[E] private[sql] (
      ctx: QueryContext,
      batchSize: Int,
      effect: ZIO[Database, E, ArraySeq[Long]],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): OptimizedBatchBatchUpdate[Nothing] =
      OptimizedBatchBatchUpdate(ctx, batchSize, effect.orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): OptimizedBatchBatchUpdate[Nothing] =
      OptimizedBatchBatchUpdate(ctx, batchSize, effect.orDieWith(f))

    def mapError[E2](f: E => E2): OptimizedBatchBatchUpdate[E2] =
      OptimizedBatchBatchUpdate(ctx, batchSize, effect.mapError(f))

    // =====| Results |=====

    def updated: ZIO[Database, E, ArraySeq[Long]] =
      effect @@ ctx.metrics.track(QueryContext.ExecutionType.AggregatedBatchUpdate(batchSize))

    def totalUpdated: ZIO[Database, E, Long] =
      updated.map(_.sum)

    def unit: ZIO[Database, E, Unit] =
      updated.unit

  }

}
