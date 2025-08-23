package oxygen.http.schema

import oxygen.predef.core.*
import oxygen.schema.AnySchema

final case class RequestPathsSchema(
    singles: ArraySeq[RequestPathsSchema.Single],
    rest: Option[RequestPathsSchema.RestParam],
)
object RequestPathsSchema {

  sealed trait Single

  final case class Const(path: String) extends Single

  final case class SingleParam(
      name: String,
      schema: AnySchema,
      doc: Option[String],
  ) extends Single

  final case class RestParam(
      name: String,
      schema: AnySchema,
      doc: Option[String],
      required: Boolean,
  )

}

final case class RequestQueryParamSchema(
    name: String,
    tpe: ParamType.Param,
    schema: AnySchema,
    doc: Option[String],
)

final case class RequestHeaderSchema(
    name: String,
    tpe: ParamType.Param,
    schema: AnySchema,
    doc: Option[String],
)

sealed trait RequestBodySchema
object RequestBodySchema {

  case object Empty extends RequestBodySchema
  sealed trait NonEmpty extends RequestBodySchema

  final case class Single(
      name: String,
      schema: AnySchema,
      doc: Option[String],
  ) extends NonEmpty

}
