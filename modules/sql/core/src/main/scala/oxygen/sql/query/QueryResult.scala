package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.*
import oxygen.sql.error.*
import oxygen.zio.instances.given
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

    def singleOrElse[E2 >: E](error: => E2): ZIO[Database, E2, A] = singleOrElse[E2] { _ => error }
    def singleOrElse[E2 >: E](error: Int => E2): ZIO[Database, E2, A] =
      this.to[List].flatMap {
        case row :: Nil => ZIO.succeed(row)
        case rows       => ZIO.fail(error(rows.size))
      }

    def single(using ev: QueryError <:< E): ZIO[Database, E, A] =
      this.singleOrElse { actualSize => ev(QueryError(ctx, QueryError.InvalidResultSetSize(QueryError.InvalidResultSetSize.ExpectedSize.Single, actualSize))) }

    def optionOrElse[E2 >: E](error: => E2): ZIO[Database, E2, Option[A]] = optionOrElse[E2] { _ => error }
    def optionOrElse[E2 >: E](error: Int => E2): ZIO[Database, E2, Option[A]] =
      this.to[List].flatMap {
        case row :: Nil => ZIO.some(row)
        case Nil        => ZIO.none
        case rows       => ZIO.fail(error(rows.size))
      }

    def option(using ev: QueryError <:< E): ZIO[Database, E, Option[A]] =
      optionOrElse { actualSize => ev(QueryError(ctx, QueryError.InvalidResultSetSize(QueryError.InvalidResultSetSize.ExpectedSize.Optional, actualSize))) }

    def chunk: ZIO[Database, E, Chunk[A]] = this.to[Chunk]
    def contiguous: ZIO[Database, E, Contiguous[A]] = this.to[Contiguous]

    // =====| Root Result Functions |=====

    def to[S[_]: SeqOps as ops]: ZIO[Database, E, S[A]] =
      this.effect.run(Sinks.seq[S, A]) @@ ctx.metrics.track(QueryContext.ExecutionType.Query)

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
      effect: ZIO[Database, E, Contiguous[Int]],
  ) {

    // =====| Error Mapping |=====

    def orDie(using ev1: E IsSubtypeOfError Throwable, ev2: CanFail[E]): BatchUpdate[Nothing] =
      BatchUpdate(ctx, batchSize, effect.orDie)

    def orDieWith(f: E => Throwable)(using ev1: CanFail[E]): BatchUpdate[Nothing] =
      BatchUpdate(ctx, batchSize, effect.orDieWith(f))

    def mapError[E2](f: E => E2): BatchUpdate[E2] =
      BatchUpdate(ctx, batchSize, effect.mapError(f))

    // =====| Results |=====

    def updated: ZIO[Database, E, Contiguous[Int]] =
      effect @@ ctx.metrics.track(QueryContext.ExecutionType.JdbcBatchUpdate(batchSize))

    def totalUpdated: ZIO[Database, E, Int] =
      updated.map(_.sum)

    def unit: ZIO[Database, E, Unit] =
      updated.unit

    // TODO (KR) : validate number of updated rows

  }

}
