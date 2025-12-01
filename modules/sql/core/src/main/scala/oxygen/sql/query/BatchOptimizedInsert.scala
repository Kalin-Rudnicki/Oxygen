package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.Database
import oxygen.sql.error.QueryError
import oxygen.sql.schema.*
import oxygen.zio.instances.chunkSeqOps
import scala.collection.mutable
import zio.*
import zio.stream.*

final case class BatchOptimizedInsert[I] private (
    underlyingQuery: QueryI[I],
    sqlParts: BatchOptimizedInsert.SqlParts,
) {

  val singleValueSize: Int = underlyingQuery.encoder.size
  val maxAllowedValues: Int = BatchOptimizedInsert.maxPsqlPreparedStatementArgs / singleValueSize

  object builtQuery {

    def ofNumValues(numValues: Int): BatchOptimizedInsert.BatchQuery[I] =
      BatchOptimizedInsert.BatchQuery(
        QueryI[Chunk[I]](
          ctx = underlyingQuery.ctx.copy(sql = sqlParts.toQueryOfSize(numValues)),
          encoder = InputEncoder.BatchChunkEncoder(underlyingQuery.encoder, numValues),
        ),
        numValues,
      )

    def withMaxTotalArgs(numPreparedStatementArgs: Int): BatchOptimizedInsert.BatchQuery[I] =
      ofNumValues(numPreparedStatementArgs / singleValueSize)

    lazy val maxPsqlQuery: BatchOptimizedInsert.BatchQuery[I] =
      withMaxTotalArgs(BatchOptimizedInsert.maxPsqlPreparedStatementArgs)

  }

  object insert {

    private def batchChunkInternal(vs: Chunk[I]): ZIO[Database, QueryError, Long] =
      if vs.length == maxAllowedValues then ZIO.logDebug(s"Inserting max batch size of $maxAllowedValues") *> builtQuery.maxPsqlQuery.execute(vs)
      else ZIO.logDebug(s"Inserting non-max batch size of ${vs.length}") *> builtQuery.ofNumValues(vs.length).execute(vs)

    private def megaBatchInternal(vs: Chunk[Chunk[I]]): ZIO[Database, QueryError, Long] =
      ZIO.logDebug(s"Inserting mega max batch size of $maxAllowedValues * jdbc batch size of ${vs.length}") *> builtQuery.maxPsqlQuery.executeMega(vs)

    private def prepareStream[R, E](s: ZStream[R, E, I]): ZStream[R, E, Chunk[I]] =
      s.rechunk(maxAllowedValues).chunks

    private def streamInternal[R, E](
        vs: ZStream[R, E, I],
        cfg: BatchOptimizedInsert.StreamConfig,
        mapEffect: ZIO[Database, QueryError, Long] => ZIO[Database, E, Long],
    ): ZIO[R & Database, E, Long] =
      (cfg.par, cfg.batchOfBatch) match {
        case (Some(par), None) =>
          prepareStream(vs).mapZIOParUnordered(par.n, par.bufferSize) { vs => mapEffect(batchChunkInternal(vs)) }.runSum
        case (None, None) =>
          prepareStream(vs).mapZIO { vs => mapEffect(batchChunkInternal(vs)) }.runSum
        case (Some(par), Some(jdbcBatchSize)) =>

          ZIO.scoped {
            for {
              (isMaxSize, isNotMaxSize) <- prepareStream(vs).partition(_.length == maxAllowedValues, 2)
              consumeIsMaxSize = isMaxSize.rechunk(jdbcBatchSize).chunks.mapZIOParUnordered(par.n, par.bufferSize) { vs => mapEffect(megaBatchInternal(vs)) }.runSum
              consumeIsNotMaxSize = isNotMaxSize.mapZIOParUnordered(par.n, par.bufferSize) { vs => mapEffect(batchChunkInternal(vs)) }.runSum
              (c1, c2) <- consumeIsMaxSize <&> consumeIsNotMaxSize
              _ <- ZIO.logDebug(s"total updated (max size mega aggregation) = $c1, total updated (other) = $c2")
            } yield c1 + c2
          }
        case (None, Some(jdbcBatchSize)) =>
          ZIO.scoped {
            for {
              (isMaxSize, isNotMaxSize) <- prepareStream(vs).partition(_.length == maxAllowedValues, 2)
              consumeIsMaxSize = isMaxSize.rechunk(jdbcBatchSize).chunks.mapZIO { vs => mapEffect(megaBatchInternal(vs)) }.runSum
              consumeIsNotMaxSize = isNotMaxSize.mapZIO { vs => mapEffect(batchChunkInternal(vs)) }.runSum
              (c1, c2) <- consumeIsMaxSize <&> consumeIsNotMaxSize
              _ <- ZIO.logDebug(s"total updated (max size mega aggregation) = $c1, total updated (other) = $c2")
            } yield c1 + c2
          }
      }

    // TODO (KR) : support:
    //           : - exp size (% complete)

    def all(vs: I*): ZIO[Database, QueryError, Long] =
      chunk(Chunk(vs*))

    def chunk(
        vs: Chunk[I],
        cfg: BatchOptimizedInsert.StreamConfig => BatchOptimizedInsert.StreamConfig = identity,
    ): ZIO[Database, QueryError, Long] =
      if vs.length <= maxAllowedValues then batchChunkInternal(vs) else stream(ZStream.fromChunk(vs), cfg)

    def seq[S[_]: SeqRead](
        vs: S[I],
        cfg: BatchOptimizedInsert.StreamConfig => BatchOptimizedInsert.StreamConfig = identity,
    ): ZIO[Database, QueryError, Long] =
      chunk(vs.into[Chunk], cfg)

    def stream[R, E >: QueryError](
        vs: ZStream[R, E, I],
        cfg: BatchOptimizedInsert.StreamConfig => BatchOptimizedInsert.StreamConfig = identity,
    ): ZIO[R & Database, E, Long] =
      streamInternal(vs, cfg(BatchOptimizedInsert.StreamConfig.default), identity)

    def streamDbDie[R, E](
        vs: ZStream[R, E, I],
        cfg: BatchOptimizedInsert.StreamConfig => BatchOptimizedInsert.StreamConfig = identity,
    ): ZIO[R & Database, E, Long] =
      streamInternal(vs, cfg(BatchOptimizedInsert.StreamConfig.default), _.orDie)

  }

}
object BatchOptimizedInsert {

  final case class SqlParts(
      beforeValues: String,
      values: String,
      afterValues: String,
  ) {

    private val separator = ", "

    def toQueryOfSize(size: Int): String = {
      if size < 1 then throw new RuntimeException(s"WTF are you doing trying to create a query of (size = $size) < 1")

      val builder = mutable.StringBuilder(beforeValues.length + afterValues.length + values.length * size + separator.length * (size - 1))

      builder.append(beforeValues)

      builder.append(values)

      var i: Int = 1
      while i < size do {
        builder.append(separator)
        builder.append(values)
        i = i + 1
      }

      builder.append(afterValues)

      builder.toString()
    }

  }
  object SqlParts {

    private val reg = "(?s)\\A(.*)(\\([?, \n]+\\))(.*)?\\z".r

    def unsafeParse(sql: String): SqlParts = sql match
      case reg(before, values, after) => SqlParts(before, values, after)
      case _                          => throw new RuntimeException(s"Unable to parse insert query for the purpose of batch insert:\n\n$sql")

  }

  final class BatchQuery[I](
      query: QueryI[Chunk[I]],
      val numValues: Int,
  ) {

    def execute(values: Chunk[I]): ZIO[Database, QueryError, Long] =
      query.optimizedBatch(numValues, values).updated

    def executeMega(values: Chunk[Chunk[I]]): ZIO[Database, QueryError, Long] =
      query.optimizedBatchBatch(numValues * values.length, values).totalUpdated

  }

  final class StreamConfig private (
      private[BatchOptimizedInsert] val par: Option[StreamConfig.Par],
      private[BatchOptimizedInsert] val batchOfBatch: Option[Int],
  ) {

    def sequential: StreamConfig = StreamConfig(None, batchOfBatch)
    def parallel(n: Int): StreamConfig = StreamConfig(StreamConfig.Par(n, 16).some, batchOfBatch)
    def parallel(n: Int, bufferSize: Int): StreamConfig = StreamConfig(StreamConfig.Par(n, bufferSize).some, batchOfBatch)

    def withAdditionalJdbcBatchSize(size: Int): StreamConfig = StreamConfig(par, size.some)

  }
  object StreamConfig {

    final case class Par(n: Int, bufferSize: Int)

    val default: StreamConfig = StreamConfig(None, None)

  }

  val maxPsqlPreparedStatementArgs: Int = 32_767

  def unsafeParse[I](underlyingQuery: QueryI[I]): BatchOptimizedInsert[I] =
    BatchOptimizedInsert(underlyingQuery, SqlParts.unsafeParse(underlyingQuery.ctx.sql))

}
