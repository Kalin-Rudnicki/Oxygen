package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.sql.{Database, DbConfig}
import oxygen.sql.error.{ConnectionError, QueryError}
import oxygen.sql.schema.{InputEncoder, ResultDecoder}
import scala.annotation.tailrec
import zio.stream.*

private[sql] final class PreparedStatement private (
    ctx: QueryContext,
    rawPS: java.sql.PreparedStatement,
) {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def executeUpdate: IO[QueryError, Int] =
    attemptExecute { rawPS.executeUpdate() }

  def executeUpdate[I](inputEncoder: InputEncoder[I], input: I): IO[QueryError, Int] =
    writeInput(inputEncoder, input) *>
      executeUpdate

  def executeBatchUpdate[I](inputEncoder: InputEncoder[I], inputs: Chunk[I]): IO[QueryError, ArraySeq[Int]] =
    writeInputs(inputEncoder, inputs) *>
      attemptExecute { rawPS.executeBatch() }.map(ArraySeq.unsafeWrapArray)

  def executeQuery[O](resultDecoder: ResultDecoder[O]): ZStream[Database, QueryError, O] =
    ZStream.scoped { executeResult <*> Database.executionConfig }.flatMap { case (rawRS, exeCfg) =>
      makeStream(rawRS, resultDecoder, exeCfg)
    }

  def executeQuery[I, O](inputEncoder: InputEncoder[I], input: I, resultDecoder: ResultDecoder[O]): ZStream[Database, QueryError, O] =
    ZStream.scoped { (writeInput(inputEncoder, input) *> executeResult) <*> Database.executionConfig }.flatMap { case (rawRS, exeCfg) =>
      makeStream(rawRS, resultDecoder, exeCfg)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val writer: InputWriter = InputWriter(rawPS)

  private inline def attemptJava[A](action: String)(thunk: => A): IO[QueryError, A] =
    ZIO.attempt { thunk }.mapError { cause => QueryError(ctx, QueryError.JDBCError(action, cause)) }

  private inline def attemptExecute[A](thunk: => A): IO[QueryError, A] =
    ZIO.attempt { thunk }.mapError { cause => QueryError(ctx, QueryError.Cause.fromThrowable(cause)) }

  private def writeInput[I](inputEncoder: InputEncoder[I], input: I): IO[QueryError, Unit] =
    attemptJava("write inputs") { inputEncoder.unsafeEncode(writer, input) }

  private def writeInputs[I](inputEncoder: InputEncoder[I], inputs: Chunk[I]): IO[QueryError, Unit] =
    ZIO.foreachDiscard(inputs) { input =>
      writeInput(inputEncoder, input) *>
        attemptJava("put batch") { writer.putBatch() }
    }

  private def unsafeReadResultSet(rawRS: java.sql.ResultSet): ArraySeq[Any] = {
    val size: Int = rawRS.getMetaData.getColumnCount
    var idx: Int = 0
    val array: Array[Any] = new Array[Any](size)
    while (idx < size) {
      val idx2 = idx + 1
      array(idx) = rawRS.getObject(idx2)
      idx = idx2
    }
    ArraySeq.unsafeWrapArray(array)
  }

  private def readRawResults(
      rawRS: java.sql.ResultSet,
      bufferChunkSize: NonEmptyList[Int],
  ): ZChannel[Any, Any, Any, Any, QueryError, ArraySeq[ArraySeq[Any]], Unit] = {
    val builder = ArraySeq.newBuilder[ArraySeq[Any]]
    builder.sizeHint(bufferChunkSize.head)

    @tailrec
    def loop2(idx: Int, bufferSize: Int, hasNext: => Boolean): (ZChannel[Any, Any, Any, Any, QueryError, ArraySeq[ArraySeq[Any]], Unit], Boolean) =
      if (idx < bufferSize)
        if (hasNext) {
          builder.addOne(unsafeReadResultSet(rawRS))
          loop2(idx + 1, bufferSize, rawRS.next())
        } else
          (ZChannel.write(builder.result()) *> ZChannel.unit, false)
      else
        (ZChannel.write(builder.result()), true)

    def loop1(bufferChunkSize: NonEmptyList[Int]): ZChannel[Any, Any, Any, Any, QueryError, ArraySeq[ArraySeq[Any]], Unit] =
      if (bufferChunkSize.head > 0)
        ZChannel.suspend {
          builder.clear()
          val (current, doLoop) =
            if (rawRS.next()) {
              try {
                loop2(0, bufferChunkSize.head, true)
              } catch {
                case e: Throwable => (ZChannel.write(builder.result()) *> ZChannel.fail(QueryError(ctx, QueryError.Cause.fromThrowable(e))), false)
              }
            } else
              (ZChannel.unit, false)

          if (doLoop) {
            val newBufferSize: NonEmptyList[Int] = bufferChunkSize.tail match
              case Nil          => bufferChunkSize
              case head :: tail => builder.sizeHint(head); NonEmptyList(head, tail)
            current *> loop1(newBufferSize)
          } else
            current
        }
      else
        ZChannel.fromZIO { ZIO.dieMessage(s"bufferSize (${bufferChunkSize.head}) <= 0 ") }

    loop1(bufferChunkSize)
  }

  private def convertRawResultChunk[O](rawResults: ArraySeq[ArraySeq[Any]], resultDecoder: ResultDecoder[O]): ZChannel[Any, Any, Any, Any, QueryError, Chunk[O], Unit] =
    rawResults.length match {
      case 0 =>
        ZChannel.write(Chunk.empty)
      case 1 =>
        resultDecoder.decode(rawResults(0)) match {
          case Right(value) => ZChannel.write(Chunk.single(value))
          case Left(error)  => ZChannel.fail(QueryError(ctx, error))
        }
      case len =>
        val builder = Chunk.newBuilder[O]
        builder.sizeHint(len)

        @tailrec
        def loop(idx: Int): ZChannel[Any, Any, Any, Any, QueryError, Chunk[O], Unit] =
          if (idx < len)
            resultDecoder.decode(rawResults(idx)) match {
              case Right(value) => builder.addOne(value); loop(idx + 1)
              case Left(error)  => ZChannel.write(builder.result()) *> ZChannel.fail(QueryError(ctx, error))
            }
          else
            ZChannel.write(builder.result())

        loop(0)
    }

  private def makeStream[O](rawRS: java.sql.ResultSet, resultDecoder: ResultDecoder[O], exeCfg: DbConfig.Execution): Stream[QueryError, O] = {
    val baseStream: Stream[QueryError, O] = ZStream.fromChannel { readChannel(rawRS, resultDecoder, exeCfg.bufferChunkSize) }

    exeCfg.bufferNumChunks match
      case Some(bufferNumChunks) => baseStream.bufferChunks(bufferNumChunks)
      case None                  => baseStream
  }

  private def readChannel[O](rawRS: java.sql.ResultSet, resultDecoder: ResultDecoder[O], bufferChunkSize: NonEmptyList[Int]): ZChannel[Any, Any, Any, Any, QueryError, Chunk[O], Unit] = {
    // TODO (KR) : make the read single head behavior configurable?
    val (current, doLoop) =
      try {
        if (rawRS.next())
          resultDecoder.decode(unsafeReadResultSet(rawRS)) match {
            case Right(value) => (ZChannel.write(Chunk.single(value)), true)
            case Left(error)  => (ZChannel.fail(QueryError(ctx, error)), false)
          }
        else
          (ZChannel.unit, false)
      } catch {
        case e: Throwable => (ZChannel.fail(QueryError(ctx, QueryError.Cause.fromThrowable(e))), false)
      }

    if (doLoop)
      current *> readRawResults(rawRS, bufferChunkSize).flatMapOut(convertRawResultChunk(_, resultDecoder))
    else
      current
  }

  extension [Env1, InErr, InElem, InDone, OutErr1, OutElem1, OutDone](self: ZChannel[Env1, InErr, InElem, InDone, OutErr1, OutElem1, OutDone])
    private def flatMapOut[Env2 <: Env1, OutErr2 >: OutErr1, OutElem2](
        f: OutElem1 => ZChannel[Env2, Any, Any, Any, OutErr2, OutElem2, Unit],
    ): ZChannel[Env2, InErr, InElem, InDone, OutErr2, OutElem2, OutDone] = {
      lazy val reader: ZChannel[Env2, OutErr1, OutElem1, OutDone, OutErr2, OutElem2, OutDone] =
        ZChannel.readWithCause(
          (out: OutElem1) => f(out) *> reader,
          (e: Cause[OutErr1]) => ZChannel.refailCause(e),
          (z: OutDone) => ZChannel.succeedNow(z),
        )

      self >>> reader
    }

  private def executeResult: ZIO[Scope, QueryError, java.sql.ResultSet] =
    (for {
      rawRS <- attemptExecute { rawPS.executeQuery() }
      _ <- ZIO.addFinalizer { attemptExecute { rawRS.close() }.orDie }
    } yield rawRS).uninterruptible

  private def setFetchSizeScoped(fetchSize: Int): ZIO[Scope, ConnectionError, Unit] =
    ZIO
      .attempt { rawPS.setFetchSize(fetchSize) }
      .mapError(ConnectionError(_))
      .withFinalizer { _ => ZIO.attempt { rawPS.setFetchSize(0) }.orDieWith(ConnectionError(_)) }

}
object PreparedStatement {

  def prepare(ctx: QueryContext, fetchSize: Option[Int]): ZIO[Database & Scope, QueryError, PreparedStatement] =
    (for {
      database <- ZIO.service[Database]
      _ <- database.logQuery(ctx)
      (con, conTpe) <- database.getConnectionAndType.mapError(QueryError.Connection(_))
      rawPS <-
        ZIO
          .attempt { con.connection.prepareStatement(ctx.sql) }
          .mapError(QueryError.JDBCError("create prepared statement", _))
          .withFinalizer { rawPS => ZIO.attempt { rawPS.close() }.orDieWith(e => QueryError(ctx, QueryError.JDBCError("close prepared statement", e))) }

      ps = PreparedStatement(ctx, rawPS)

      _ <- ZIO
        .foreachDiscard(fetchSize) { fetchSize =>
          conTpe match {
            case Database.ConnectionState.ConnectionType.Transactionless  => con.disableAutoCommitScoped *> ps.setFetchSizeScoped(fetchSize)
            case _: Database.ConnectionState.ConnectionType.Transactional => ps.setFetchSizeScoped(fetchSize)
          }
        }
        .mapError(QueryError.JDBCError("setting fetch size", _))
    } yield ps).uninterruptible.mapError(QueryError(ctx, _))

}
