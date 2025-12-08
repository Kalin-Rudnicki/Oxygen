package oxygen.http.schema.partial

import oxygen.http.schema.{ParamType, RequestBodySchema, RequestHeaderSchema, RequestPathsSchema, RequestQueryParamSchema, RequestSchema}
import oxygen.predef.core.*
import scala.annotation.tailrec
import zio.http.Method

final case class RequestSchemaAggregator private (
    method: Method,
    paths: NonEmptyList[Growable[PartiallyAppliedPathSchema]],
    queryParams: Growable[RequestQueryParamSchema],
    headers: Growable[RequestHeaderSchema],
    bodies: Growable[RequestBodySchema],
) {

  def >>>(that: RequestSchemaAggregator): RequestSchemaAggregator =
    RequestSchemaAggregator(
      method = this.method ++ that.method,
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

  // TODO (KR) : Support a more robust version of what `nonPath || nonPath` means.
  //           : Right now, `nonPath || nonPath` and `nonPath >>> nonPath` look the same.
  def ||(that: RequestSchemaAggregator): RequestSchemaAggregator =
    RequestSchemaAggregator(
      method = this.method ++ that.method, // what to do here...
      paths = this.paths ++ that.paths,
      queryParams = this.queryParams ++ that.queryParams,
      headers = this.headers ++ that.headers,
      bodies = this.bodies ++ that.bodies,
    )

  def unsafeBuild(
      apiName: String,
      endpointName: String,
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
          throw RequestSchemaAggregator.InvalidSchemaError(apiName, endpointName, RequestSchemaAggregator.InvalidSchemaError.Cause.RestPathIsNotInTailPosition)
      }

    RequestSchema(
      method = method.someWhen(_ != Method.ANY),
      paths = paths.map { p => compilePath(p.to[List], Growable.empty) },
      queryParams = queryParams.toArraySeq,
      headers = headers.toArraySeq,
      body = bodies.collect { case b: RequestBodySchema.NonEmpty => b }.to[List] match {
        case Nil         => RequestBodySchema.Empty
        case body :: Nil => body

        // TODO (KR) : consider doing something better than throwing
        case _ =>
          throw RequestSchemaAggregator.InvalidSchemaError(apiName, endpointName, RequestSchemaAggregator.InvalidSchemaError.Cause.MultipleBodies)
      },
    )
  }

}
object RequestSchemaAggregator {

  private def make(
      method: Method = Method.ANY,
      paths: NonEmptyList[Growable[PartiallyAppliedPathSchema]] = NonEmptyList.one(Growable.empty),
      queryParams: Growable[RequestQueryParamSchema] = Growable.empty,
      headers: Growable[RequestHeaderSchema] = Growable.empty,
      bodies: Growable[RequestBodySchema] = Growable.empty,
  ): RequestSchemaAggregator =
    RequestSchemaAggregator(
      method = method,
      paths = paths,
      queryParams = queryParams,
      headers = headers,
      bodies = bodies,
    )

  val empty: RequestSchemaAggregator = make()
  private[http] def method(method: Method): RequestSchemaAggregator = make(method = method)
  private[http] def paths(paths: NonEmptyList[Growable[PartiallyAppliedPathSchema]]): RequestSchemaAggregator = make(paths = paths)
  private[http] def simplePath(path: PartiallyAppliedPathSchema): RequestSchemaAggregator = paths(NonEmptyList.one(Growable.single(path)))
  private[http] def query(query: RequestQueryParamSchema): RequestSchemaAggregator = make(queryParams = Growable.single(query))
  private[http] def header(header: RequestHeaderSchema): RequestSchemaAggregator = make(headers = Growable.single(header))
  private[http] def body(body: RequestBodySchema): RequestSchemaAggregator = make(bodies = Growable.single(body))

  final case class InvalidSchemaError(
      apiName: String,
      endpointName: String,
      cause: InvalidSchemaError.Cause,
  ) extends Throwable {

    override def getMessage: String =
      s"Invalid request schema for `$apiName`.`$endpointName`: $cause"

  }
  object InvalidSchemaError {

    enum Cause {
      case RestPathIsNotInTailPosition
      case MultipleBodies
    }

  }

}
