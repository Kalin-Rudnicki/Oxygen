package oxygen.sql.query

import oxygen.predef.core.*
import oxygen.sql.error.*
import oxygen.sql.query.dsl.CompileMacros
import oxygen.sql.schema.*
import oxygen.zio.instances.given
import zio.{Chunk, ZIO}
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
    encoder: Option[InputEncoder[Any]],
) extends QueryLike { self =>

  def apply(): QueryResult.Update[QueryError] = execute()
  def execute(): QueryResult.Update[QueryError] =
    QueryResult.Update[QueryError](
      ctx,
      None,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx)
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

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      QueryI
//////////////////////////////////////////////////////////////////////////////////////////////////////

final class QueryI[I](
    val ctx: QueryContext,
    encoder: InputEncoder[I],
) extends QueryLike { self =>

  def apply(input: I): QueryResult.Update[QueryError] = execute(input)
  def execute(input: I): QueryResult.Update[QueryError] =
    QueryResult.Update[QueryError](
      ctx,
      None,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx)
          updated <- ps.executeUpdate(encoder, input)
        } yield updated
      },
    )

  // TODO (KR) : add some sort of hierarchy here... `object batched { def of, def seq, def stream }`

  def batched[S[_]: SeqOps](inputs: S[I]): QueryResult.BatchUpdate[QueryError] = {
    val chunk = inputs.into[Chunk]

    QueryResult.BatchUpdate[QueryError](
      ctx,
      chunk.length,
      ZIO.scoped {
        for {
          ps <- PreparedStatement.prepare(ctx)
          updated <- ps.executeBatchUpdate(encoder, chunk)
        } yield updated
      },
    )
  }

  def all(inputs: I*): QueryResult.BatchUpdate[QueryError] =
    batched(inputs)

  def apply[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2)))
  def apply[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3)))
  def apply[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4)))
  def apply[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5)))
  def apply[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5, i6)))
  def apply[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using ev: (I1, I2, I3, I4, I5, I6, I7) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5, i6, i7)))
  def apply[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def execute[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2)))
  def execute[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3)))
  def execute[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4)))
  def execute[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5)))
  def execute[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5, i6)))
  def execute[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using ev: (I1, I2, I3, I4, I5, I6, I7) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5, i6, i7)))
  def execute[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I): QueryResult.Update[QueryError] =
    self(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def contramap[I2](f: I2 => I): QueryI[I2] = QueryI[I2](self.ctx, self.encoder.contramap(f))

}
object QueryI {

  def simple[I](queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(
      encoder: InputEncoder[I],
  )(sql: String): QueryI[I] =
    QueryI(QueryContext(queryName, sql, queryType, constParams = constParams), encoder)

  inline def compile[I](inline queryName: String)(inline query: QueryI[I]): QueryI[I] = ${ CompileMacros.queryI('queryName, 'query, '{ false }) }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      QueryO
//////////////////////////////////////////////////////////////////////////////////////////////////////

final class QueryO[O](
    val ctx: QueryContext,
    encoder: Option[InputEncoder[Any]],
    decoder: ResultDecoder[O],
) extends QueryLike { self =>

  def apply(): QueryResult.Returning[QueryError, O] = execute()
  def execute(): QueryResult.Returning[QueryError, O] =
    QueryResult.Returning[QueryError, O](
      ctx,
      for {
        ps <- ZStream.scoped { PreparedStatement.prepare(ctx) }
        o <- encoder match
          case None          => ps.executeQuery(decoder)
          case Some(encoder) => ps.executeQuery(encoder, (), decoder)
      } yield o,
    )

  def map[O2](f: O => O2): QueryO[O2] = QueryO[O2](self.ctx, self.encoder, self.decoder.map(f))
  def mapOrFail[O2](f: O => Either[String, O2]): QueryO[O2] = QueryO[O2](self.ctx, self.encoder, self.decoder.mapOrFail(f))

}
object QueryO {

  def simple[O](queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(
      decoder: ResultDecoder[O],
  )(sql: String): QueryO[O] =
    QueryO(QueryContext(queryName, sql, queryType, constParams = constParams), None, decoder)

  inline def compile[O](inline queryName: String)(inline query: QueryO[O]): QueryO[O] = ${ CompileMacros.queryO('queryName, 'query, '{ false }) }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      QueryIO
//////////////////////////////////////////////////////////////////////////////////////////////////////

final class QueryIO[I, O](
    val ctx: QueryContext,
    encoder: InputEncoder[I],
    decoder: ResultDecoder[O],
) extends QueryLike { self =>

  def apply(input: I): QueryResult.Returning[QueryError, O] = execute(input)
  def execute(input: I): QueryResult.Returning[QueryError, O] =
    QueryResult.Returning[QueryError, O](
      ctx,
      for {
        ps <- ZStream.scoped { PreparedStatement.prepare(ctx) }
        o <- ps.executeQuery(encoder, input, decoder)
      } yield o,
    )

  def apply[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2)))
  def apply[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3)))
  def apply[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4)))
  def apply[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5)))
  def apply[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5, i6)))
  def apply[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using ev: (I1, I2, I3, I4, I5, I6, I7) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5, i6, i7)))
  def apply[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def execute[I1, I2](i1: I1, i2: I2)(using ev: (I1, I2) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2)))
  def execute[I1, I2, I3](i1: I1, i2: I2, i3: I3)(using ev: (I1, I2, I3) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3)))
  def execute[I1, I2, I3, I4](i1: I1, i2: I2, i3: I3, i4: I4)(using ev: (I1, I2, I3, I4) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4)))
  def execute[I1, I2, I3, I4, I5](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5)(using ev: (I1, I2, I3, I4, I5) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5)))
  def execute[I1, I2, I3, I4, I5, I6](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6)(using ev: (I1, I2, I3, I4, I5, I6) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5, i6)))
  def execute[I1, I2, I3, I4, I5, I6, I7](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7)(using ev: (I1, I2, I3, I4, I5, I6, I7) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5, i6, i7)))
  def execute[I1, I2, I3, I4, I5, I6, I7, I8](i1: I1, i2: I2, i3: I3, i4: I4, i5: I5, i6: I6, i7: I7, i8: I8)(using ev: (I1, I2, I3, I4, I5, I6, I7, I8) <:< I): QueryResult.Returning[QueryError, O] =
    self(ev((i1, i2, i3, i4, i5, i6, i7, i8)))

  def contramap[I2](f: I2 => I): QueryIO[I2, O] = QueryIO[I2, O](self.ctx, self.encoder.contramap(f), self.decoder)
  def map[O2](f: O => O2): QueryIO[I, O2] = QueryIO[I, O2](self.ctx, self.encoder, self.decoder.map(f))
  def mapOrFail[O2](f: O => Either[String, O2]): QueryIO[I, O2] = QueryIO[I, O2](self.ctx, self.encoder, self.decoder.mapOrFail(f))

}
object QueryIO {

  def simple[I, O](queryName: String, queryType: QueryContext.QueryType, constParams: (String, String)*)(
      encoder: InputEncoder[I],
      decoder: ResultDecoder[O],
  )(sql: String): QueryIO[I, O] =
    QueryIO(QueryContext(queryName, sql, queryType, constParams = constParams), encoder, decoder)

  inline def compile[I, O](inline queryName: String)(inline query: QueryIO[I, O]): QueryIO[I, O] = ${ CompileMacros.queryIO('queryName, 'query, '{ false }) }

}
