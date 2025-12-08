package oxygen.http.schema

import oxygen.schema.AnySchema

final case class ResponseHeaderSchema(
    name: String,
    tpe: ParamType.Param,
    schema: AnySchema,
    doc: Option[String],
)

sealed trait ResponseBodySchema
object ResponseBodySchema {

  case object Empty extends ResponseBodySchema
  sealed trait NonEmpty extends ResponseBodySchema

  final case class Single(
      schema: AnySchema,
  ) extends NonEmpty

  final case class ServerSentEvents(
      schema: AnySchema,
  ) extends NonEmpty

}
