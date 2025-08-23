package oxygen.http.schema.partial

import oxygen.http.schema.*
import oxygen.predef.core.*

// TODO (KR) : in the event that non-paths need to support an "or", the representation of those will need to be beefed up
//           : right now, its implicitly an AND only
final case class ResponseSchemaAggregator private (
    headers: Growable[ResponseHeaderSchema],
    bodies: Growable[ResponseBodySchema],
) {

  def >>>(that: ResponseSchemaAggregator): ResponseSchemaAggregator =
    ResponseSchemaAggregator(
      headers = this.headers ++ that.headers,
      bodies = this.bodies ++ that.bodies,
    )

  def unsafeBuild(
      apiName: String,
      routeName: String,
      statuses: ExpectedStatuses,
  ): ResponseSchema =
    ResponseSchema(
      expectedStatuses = statuses,
      headers = headers.toArraySeq,
      body = bodies.collect { case b: ResponseBodySchema.NonEmpty => b }.to[List] match {
        case Nil         => ResponseBodySchema.Empty
        case body :: Nil => body

        // TODO (KR) : consider doing something better than throwing
        case _ =>
          throw ResponseSchemaAggregator.InvalidSchemaError(apiName, routeName, ResponseSchemaAggregator.InvalidSchemaError.Cause.MultipleBodies)
      },
    )

}
object ResponseSchemaAggregator {

  private def make(
      headers: Growable[ResponseHeaderSchema] = Growable.empty,
      bodies: Growable[ResponseBodySchema] = Growable.empty,
  ): ResponseSchemaAggregator =
    ResponseSchemaAggregator(
      headers = headers,
      bodies = bodies,
    )

  private[http] val empty: ResponseSchemaAggregator = make()
  private[http] def header(header: ResponseHeaderSchema): ResponseSchemaAggregator = make(headers = Growable.single(header))
  private[http] def body(body: ResponseBodySchema): ResponseSchemaAggregator = make(bodies = Growable.single(body))

  final case class InvalidSchemaError(
      apiName: String,
      routeName: String,
      cause: InvalidSchemaError.Cause,
  ) extends Throwable {

    override def getMessage: String =
      s"Invalid response schema for `$apiName`.`$routeName`: $cause"

  }
  object InvalidSchemaError {

    enum Cause {
      case MultipleBodies
    }

  }

}
