package oxygen.http.schema.partial

import oxygen.http.core.{RequestNonPathCodec, RequestPathCodec}
import oxygen.http.schema.{ParamType, RequestBodySchema, RequestHeaderSchema, RequestPathsSchema, RequestQueryParamSchema, RequestSchema}
import oxygen.predef.core.*
import scala.annotation.tailrec
import zio.http.Method

// TODO (KR) : in the event that non-paths need to support an "or", the representation of those will need to be beefed up
//           : right now, its implicitly an AND only
final case class RequestSchemaAggregator private (
    paths: NonEmptyList[Growable[PartiallyAppliedPathSchema]],
    queryParams: Growable[RequestQueryParamSchema],
    headers: Growable[RequestHeaderSchema],
    bodies: Growable[RequestBodySchema],
) {

  def >>>(that: RequestSchemaAggregator): RequestSchemaAggregator =
    RequestSchemaAggregator(
      paths = (this.paths, that.paths) match {
        case (NonEmptyList(thisPaths, Nil), NonEmptyList(thatPaths, Nil)) => NonEmptyList.one(thisPaths ++ thatPaths)
        case _                                                            =>
          for {
            thisPaths <- this.paths
            thatPaths <- that.paths
          } yield thisPaths ++ thatPaths
      },
      queryParams = this.queryParams ++ that.queryParams,
      headers = this.headers ++ that.headers,
      bodies = this.bodies ++ that.bodies,
    )

  def unsafeBuild(
      apiName: String,
      routeName: String,
      method: Method,
  ): RequestSchema = {
    @tailrec
    def compilePath(queue: List[PartiallyAppliedPathSchema], acc: Growable[RequestPathsSchema.Single]): RequestPathsSchema =
      queue match {
        case PartiallyAppliedPathSchema.Const(path) :: tail                                          => compilePath(tail, acc :+ RequestPathsSchema.Const(path))
        case PartiallyAppliedPathSchema.Param(name, ParamType.Path.Single, schema, doc) :: tail      => compilePath(tail, acc :+ RequestPathsSchema.SingleParam(name, schema, doc))
        case PartiallyAppliedPathSchema.Param(name, ParamType.Path.Rest, schema, doc) :: Nil         => RequestPathsSchema(acc.toArraySeq, RequestPathsSchema.RestParam(name, schema, doc, false).some)
        case PartiallyAppliedPathSchema.Param(name, ParamType.Path.NonEmptyRest, schema, doc) :: Nil => RequestPathsSchema(acc.toArraySeq, RequestPathsSchema.RestParam(name, schema, doc, true).some)
        case Nil                                                                                     => RequestPathsSchema(acc.toArraySeq, None)

        // TODO (KR) : consider doing something better than throwing
        case (_: PartiallyAppliedPathSchema.Param) :: _ =>
          throw RequestSchemaAggregator.InvalidSchemaError(apiName, routeName, RequestSchemaAggregator.InvalidSchemaError.Cause.RestPathIsNotInTailPosition)
      }

    RequestSchema(
      method = method,
      paths = paths.map { p => compilePath(p.to[List], Growable.empty) },
      queryParams = queryParams.toArraySeq,
      headers = headers.toArraySeq,
      body = bodies.collect { case b: RequestBodySchema.NonEmpty => b }.to[List] match {
        case Nil         => RequestBodySchema.Empty
        case body :: Nil => body

        // TODO (KR) : consider doing something better than throwing
        case _ =>
          throw RequestSchemaAggregator.InvalidSchemaError(apiName, routeName, RequestSchemaAggregator.InvalidSchemaError.Cause.MultipleBodies)
      },
    )
  }

}
object RequestSchemaAggregator {

  private def make(
      paths: NonEmptyList[Growable[PartiallyAppliedPathSchema]] = NonEmptyList.one(Growable.empty),
      queryParams: Growable[RequestQueryParamSchema] = Growable.empty,
      headers: Growable[RequestHeaderSchema] = Growable.empty,
      bodies: Growable[RequestBodySchema] = Growable.empty,
  ): RequestSchemaAggregator =
    RequestSchemaAggregator(
      paths = paths,
      queryParams = queryParams,
      headers = headers,
      bodies = bodies,
    )

  private val empty: RequestSchemaAggregator = make()
  private[http] def paths(paths: NonEmptyList[Growable[PartiallyAppliedPathSchema]]): RequestSchemaAggregator = make(paths = paths)
  private[http] def query(query: RequestQueryParamSchema): RequestSchemaAggregator = make(queryParams = Growable.single(query))
  private[http] def header(header: RequestHeaderSchema): RequestSchemaAggregator = make(headers = Growable.single(header))
  private[http] def body(body: RequestBodySchema): RequestSchemaAggregator = make(bodies = Growable.single(body))

  def unsafeBuild(
      apiName: String,
      routeName: String,
      method: Method,
  )(
      codecs: (RequestPathCodec[?] | RequestNonPathCodec[?])*,
  ): RequestSchema = {
    val aggs: Seq[RequestSchemaAggregator] = codecs.map {
      case codec: RequestPathCodec[?]    => codec.schemaAggregator
      case codec: RequestNonPathCodec[?] => codec.schemaAggregator
    }
    aggs.foldLeft(RequestSchemaAggregator.empty) { _ >>> _ }.unsafeBuild(apiName, routeName, method)
  }

  final case class InvalidSchemaError(
      apiName: String,
      routeName: String,
      cause: InvalidSchemaError.Cause,
  ) extends Throwable {

    override def getMessage: String =
      s"Invalid request schema for `$apiName`.`$routeName`: $cause"

  }
  object InvalidSchemaError {

    enum Cause {
      case RestPathIsNotInTailPosition
      case MultipleBodies
    }

  }

}
