package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.sql.Database
import oxygen.sql.error.QueryError
import oxygen.sql.schema.{InputEncoder, ResultDecoder}
import zio.stream.ZStream

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

  def executeBatchUpdate[S[_]: SeqOps, I](inputEncoder: InputEncoder[I], inputs: S[I]): IO[QueryError, Contiguous[Int]] =
    writeInputs(inputEncoder, inputs) *>
      attemptExecute { rawPS.executeBatch() }.map(Contiguous.fromArray)

  def executeQuery[O](resultDecoder: ResultDecoder[O]): ZStream[Database & Scope, QueryError, O] =
    for {
      rawRS <- ZStream.fromZIO { executeResult }
      o <- ZStream.repeatZIOOption(readOutputsOpt(rawRS, resultDecoder)) // TODO (KR) : figure out efficient way to chunk this, special casing for 0/1?
    } yield o

  def executeQuery[I, O](inputEncoder: InputEncoder[I], input: I, resultDecoder: ResultDecoder[O]): ZStream[Scope, QueryError, O] =
    for {
      _ <- ZStream.fromZIO { writeInput(inputEncoder, input) }
      rawRS <- ZStream.fromZIO { executeResult }
      o <- ZStream.repeatZIOOption(readOutputsOpt(rawRS, resultDecoder)) // TODO (KR) : figure out efficient way to chunk this, special casing for 0/1?
    } yield o

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

  private def writeInputs[S[_]: SeqOps, I](inputEncoder: InputEncoder[I], inputs: S[I]): IO[QueryError, Unit] =
    inputs.foreachZIO { input =>
      writeInput(inputEncoder, input) *>
        attemptJava("put batch") { writer.putBatch() }
    }

  private def unsafeReadResultSet(rawRS: java.sql.ResultSet): Contiguous[Any] = {
    val size: Int = rawRS.getMetaData.getColumnCount
    var idx: Int = 1
    val builder: Contiguous.Builder[Any] = Contiguous.newBuilder
    builder.sizeHint(size)
    while (idx <= size) {
      builder.addOne(rawRS.getObject(idx))
      idx += 1
    }
    builder.result()
  }

  private def readOutputs[O](rawRS: java.sql.ResultSet, resultDecoder: ResultDecoder[O]): IO[QueryError, O] =
    for {
      values <- attemptJava("read result set") { unsafeReadResultSet(rawRS) }
      either <- attemptJava("decode values") { resultDecoder.decode(values) }
      decoded <- ZIO.fromEither(either).mapError(QueryError(ctx, _))
    } yield decoded

  private def readOutputsOpt[O](rawRS: java.sql.ResultSet, resultDecoder: ResultDecoder[O]): IO[Option[QueryError], O] =
    attemptJava("get next query result") { rawRS.next() }.asSomeError.flatMap {
      case true  => readOutputs(rawRS, resultDecoder).asSomeError
      case false => ZIO.fail(None)
    }

  private def executeResult: ZIO[Scope, QueryError, java.sql.ResultSet] =
    (for {
      rawRS <- attemptExecute { rawPS.executeQuery() }
      _ <- ZIO.addFinalizer { attemptExecute { rawRS.close() }.orDie }
    } yield rawRS).uninterruptible

}
object PreparedStatement {

  def prepare(ctx: QueryContext): ZIO[Database & Scope, QueryError, PreparedStatement] =
    (for {
      database <- ZIO.service[Database]
      _ <- database.logQuery(ctx)
      con <- database.getConnection.mapError(QueryError.Connection(_))
      rawPS <- ZIO.attempt { con.connection.prepareStatement(ctx.sql) }.mapError(QueryError.JDBCError("create prepared statement", _))
      _ <- ZIO.addFinalizer { ZIO.attempt { rawPS.close() }.mapError(e => QueryError(ctx, QueryError.JDBCError("close prepared statement", e))).orDie }
    } yield PreparedStatement(ctx, rawPS)).uninterruptible.mapError(QueryError(ctx, _))

}
