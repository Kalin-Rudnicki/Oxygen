package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.error.*
import oxygen.sql.query.dsl.CompileMacros
import oxygen.sql.schema.*
import oxygen.zio.SparseStreamAggregator
import oxygen.zio.instances.given
import zio.{Chunk, Trace, ZIO}
import zio.stream.ZStream

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Query
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait QueryLike {

  val ctx: QueryContext

  final def showQuery: IndentedString =
    IndentedString.section(s"Query< ${ctx.queryContextHeader} >")(
      IndentedString.section("sql")(ctx.sql),
    )

}

final class Query(
    val ctx: QueryContext,
    private[sql] val encoder: Option[InputEncoder[Any]],
) extends QueryLike { self =>

  def apply()(using Trace): QueryResult.Update[QueryError] = execute()
  def execute()(using Trace): QueryResult.Update[QueryError] =
    QueryResult.Update[QueryError](
      ctx,
      None,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx, None)
          updated <- encoder match
            case None          => ps.executeUpdate
            case Some(encoder) => ps.executeUpdate(encoder, ())
        } yield updated
      },
    )

}
object Query {

  def simple(queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(sql: String): Query =
    Query(QueryContext(queryName, sql, queryType, constParams = constParams), None)

  inline def compile(inline queryName: String)(inline query: Query): Query = ${ CompileMacros.query('queryName, 'query, '{ false }) }
  inline def compile(inline queryName: String, inline debug: Boolean)(inline query: Query): Query = ${ CompileMacros.query('queryName, 'query, 'debug) }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      QueryI
//////////////////////////////////////////////////////////////////////////////////////////////////////

final class QueryI[I](
    val ctx: QueryContext,
    private[sql] val encoder: InputEncoder[I],
) extends QueryLike { self =>

  def apply(input: I)(using Trace): QueryResult.Update[QueryError] = execute(input)
  def execute(input: I)(using Trace): QueryResult.Update[QueryError] =
    QueryResult.Update[QueryError](
      ctx,
      None,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx, None)
          updated <- ps.executeUpdate(encoder, input)
        } yield updated
      },
    )

  // TODO (KR) : add some sort of hierarchy here... `object batched { def of, def seq, def stream }`

  def batched[S[_]: SeqOps](inputs: S[I])(using Trace): QueryResult.BatchUpdate[QueryError] = {
    val chunk = inputs.into[Chunk]

    QueryResult.BatchUpdate[QueryError](
      ctx,
      chunk.length,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx, None)
          updated <- ps.executeBatchUpdate(encoder, chunk)
        } yield updated
      },
    )
  }

  def all(inputs: I*)(using Trace): QueryResult.BatchUpdate[QueryError] =
    batched(inputs)

  private[sql] def optimizedBatch(size: Int, input: I)(using Trace): QueryResult.OptimizedBatchUpdate[QueryError] = {
    QueryResult.OptimizedBatchUpdate[QueryError](
      ctx,
      size,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx, None)
          updated <- ps.executeUpdate(encoder, input)
        } yield updated
      },
    )
  }

  private[sql] def optimizedBatchBatch(size: Int, input: Chunk[I])(using Trace): QueryResult.OptimizedBatchBatchUpdate[QueryError] = {
    QueryResult.OptimizedBatchBatchUpdate[QueryError](
      ctx,
      size,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx, None)
          updated <- ps.executeBatchUpdate(encoder, input)
        } yield updated.map(_.toLong)
      },
    )
  }

  def apply[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2)))
  def apply[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3)))
  def apply[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4)))
  def apply[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5)))
  def apply[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5, i6)))
  def apply[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using ev: (I1, I2, I3, I4, I5, I6, I7) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7)))
  def apply[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using
      ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I,
      trace: Trace,
  ): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def execute[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2)))
  def execute[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3)))
  def execute[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4)))
  def execute[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5)))
  def execute[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5, i6)))
  def execute[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using ev: (I1, I2, I3, I4, I5, I6, I7) <:< I, trace: Trace): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7)))
  def execute[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using
      ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I,
      trace: Trace,
  ): QueryResult.Update[QueryError] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def contramap[I2](f: I2 => I): QueryI[I2] = QueryI[I2](self.ctx, self.encoder.contramap(f))

}
object QueryI {

  def simple[I](queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(
      encoder: InputEncoder[I],
  )(sql: String): QueryI[I] =
    QueryI(QueryContext(queryName, sql, queryType, constParams = constParams), encoder)

  inline def compile[I](inline queryName: String)(inline query: QueryI[I]): QueryI[I] = ${ CompileMacros.queryI('queryName, 'query, '{ false }) }
  inline def compile[I](inline queryName: String, inline debug: Boolean)(inline query: QueryI[I]): QueryI[I] = ${ CompileMacros.queryI('queryName, 'query, 'debug) }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      QueryO
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait QueryO[O] extends QueryLike {

  val ctx: QueryContext

  final def apply()(using Trace): QueryResult.Returning[QueryError, O] = execute()
  def execute()(using Trace): QueryResult.Returning[QueryError, O]

  def map[O2](f: O => O2): QueryO[O2] = QueryO.MapOutput(this, f)
  def mapOrFail[O2](f: O => Either[String, O2]): QueryO[O2] = QueryO.MapOrFailOutput(this, f)
  final def >>>[O2](agg: SparseStreamAggregator[O, O2]): QueryO[O2] = QueryO.Agg(this, agg)

}
object QueryO {

  def simple[O](queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(
      decoder: ResultDecoder[O],
  )(sql: String): QueryO[O] =
    QueryO.Simple(QueryContext(queryName, sql, queryType, constParams = constParams), None, decoder)

  inline def compile[O](inline queryName: String)(inline query: QueryO[O]): QueryO[O] = ${ CompileMacros.queryO('queryName, 'query, '{ false }) }
  inline def compile[O](inline queryName: String, inline debug: Boolean)(inline query: QueryO[O]): QueryO[O] = ${ CompileMacros.queryO('queryName, 'query, 'debug) }

  final class Simple[O](
      val ctx: QueryContext,
      private[sql] val encoder: Option[InputEncoder[Any]],
      private[sql] val decoder: ResultDecoder[O],
  ) extends QueryO[O] { self =>

    override def execute()(using Trace): QueryResult.Returning[QueryError, O] =
      QueryResult.Returning[QueryError, O](
        ctx,
        cfg =>
          for {
            ps <- ZStream.scoped { PreparedStatement.prepare(ctx, cfg.fetchSize) }
            o <- encoder match
              case None          => ps.executeQuery(decoder)
              case Some(encoder) => ps.executeQuery(encoder, (), decoder)
          } yield o,
      )

    override def map[O2](f: O => O2): QueryO[O2] = QueryO.Simple[O2](self.ctx, self.encoder, self.decoder.map(f))
    override def mapOrFail[O2](f: O => Either[String, O2]): QueryO[O2] = QueryO.Simple[O2](self.ctx, self.encoder, self.decoder.mapOrFail(f))

  }

  final case class MapOutput[InnerO, O](
      inner: QueryO[InnerO],
      f: InnerO => O,
  ) extends QueryO[O] {

    override val ctx: QueryContext = inner.ctx

    override def execute()(using Trace): QueryResult.Returning[QueryError, O] =
      inner.execute().map(f)

  }

  final case class MapOrFailOutput[InnerO, O](
      inner: QueryO[InnerO],
      f: InnerO => Either[String, O],
  ) extends QueryO[O] {

    override val ctx: QueryContext = inner.ctx

    override def execute()(using Trace): QueryResult.Returning[QueryError, O] =
      inner.execute().mapOrFail(f)

  }

  final case class Agg[InnerO, O](
      inner: QueryO[InnerO],
      agg: SparseStreamAggregator[InnerO, O],
  ) extends QueryO[O] {

    override val ctx: QueryContext = inner.ctx

    override def execute()(using Trace): QueryResult.Returning[QueryError, O] =
      inner.execute() >>> agg

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      QueryIO
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait QueryIO[I, O] extends QueryLike { self =>

  val ctx: QueryContext

  final def apply(input: I)(using Trace): QueryResult.Returning[QueryError, O] = execute(input)
  def execute(input: I)(using Trace): QueryResult.Returning[QueryError, O]

  final def apply[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2)))
  final def apply[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3)))
  final def apply[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4)))
  final def apply[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5)))
  final def apply[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5, i6)))
  final def apply[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using
      ev: (I1, I2, I3, I4, I5, I6, I7) <:< I,
      trace: Trace,
  ): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7)))
  final def apply[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using
      ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I,
      trace: Trace,
  ): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  final def execute[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2)))
  final def execute[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3)))
  final def execute[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4)))
  final def execute[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5)))
  final def execute[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I, trace: Trace): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5, i6)))
  final def execute[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using
      ev: (I1, I2, I3, I4, I5, I6, I7) <:< I,
      trace: Trace,
  ): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7)))
  final def execute[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using
      ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I,
      trace: Trace,
  ): QueryResult.Returning[QueryError, O] =
    self.execute(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def contramap[I2](f: I2 => I): QueryIO[I2, O]
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO.MapOutput(this, f)
  def mapOrFail[O2](f: O => Either[String, O2]): QueryIO[I, O2] = QueryIO.MapOrFailOutput(this, f)
  final def >>>[O2](agg: SparseStreamAggregator[O, O2]): QueryIO[I, O2] = QueryIO.Agg(this, agg)

}
object QueryIO {

  def simple[I, O](queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(
      encoder: InputEncoder[I],
      decoder: ResultDecoder[O],
  )(sql: String): QueryIO[I, O] =
    QueryIO.Simple(QueryContext(queryName, sql, queryType, constParams = constParams), encoder, decoder)

  inline def compile[I, O](inline queryName: String)(inline query: QueryIO[I, O]): QueryIO[I, O] = ${ CompileMacros.queryIO('queryName, 'query, '{ false }) }
  inline def compile[I, O](inline queryName: String, inline debug: Boolean)(inline query: QueryIO[I, O]): QueryIO[I, O] = ${ CompileMacros.queryIO('queryName, 'query, 'debug) }

  final class Simple[I, O](
      val ctx: QueryContext,
      private[sql] val encoder: InputEncoder[I],
      private[sql] val decoder: ResultDecoder[O],
  ) extends QueryIO[I, O] { self =>

    override def execute(input: I)(using Trace): QueryResult.Returning[QueryError, O] =
      QueryResult.Returning[QueryError, O](
        ctx,
        cfg =>
          for {
            ps <- ZStream.scoped { PreparedStatement.prepare(ctx, cfg.fetchSize) }
            o <- ps.executeQuery(encoder, input, decoder)
          } yield o,
      )

    override def contramap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO.Simple[I2, O](self.ctx, self.encoder.contramap(f), self.decoder)
    override def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO.Simple[I, O2](self.ctx, self.encoder, self.decoder.map(f))
    override def mapOrFail[O2](f: O => Either[String, O2]): QueryIO[I, O2] = QueryIO.Simple[I, O2](self.ctx, self.encoder, self.decoder.mapOrFail(f))

  }

  final case class MapOutput[I, InnerO, O](
      inner: QueryIO[I, InnerO],
      f: InnerO => O,
  ) extends QueryIO[I, O] {

    override val ctx: QueryContext = inner.ctx

    override def execute(input: I)(using Trace): QueryResult.Returning[QueryError, O] =
      inner.execute(input).map(f)

    override def contramap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO.MapOutput(inner.contramap(f), this.f)

  }

  final case class MapOrFailOutput[I, InnerO, O](
      inner: QueryIO[I, InnerO],
      f: InnerO => Either[String, O],
  ) extends QueryIO[I, O] {

    override val ctx: QueryContext = inner.ctx

    override def execute(input: I)(using Trace): QueryResult.Returning[QueryError, O] =
      inner.execute(input).mapOrFail(f)

    override def contramap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO.MapOrFailOutput(inner.contramap(f), this.f)

  }

  final case class Agg[I, InnerO, O](
      inner: QueryIO[I, InnerO],
      agg: SparseStreamAggregator[InnerO, O],
  ) extends QueryIO[I, O] {

    override val ctx: QueryContext = inner.ctx

    override def execute(input: I)(using Trace): QueryResult.Returning[QueryError, O] =
      inner.execute(input) >>> agg

    override def contramap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO.Agg(inner.contramap(f), this.agg)

  }

}
