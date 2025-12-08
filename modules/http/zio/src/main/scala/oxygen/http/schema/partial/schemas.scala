package oxygen.http.schema.partial

import oxygen.http.schema.ParamType
import oxygen.schema.AnySchema

final case class PartialPathSchema(
    schema: AnySchema,
    tpe: ParamType.Path,
)

final case class PartialParamSchema(
    schema: AnySchema,
    tpe: ParamType.Param,
)

enum PartialBodySchema {
  case Empty
  case Single(schema: AnySchema)
  case ServerSentEvents(schema: AnySchema)
}
