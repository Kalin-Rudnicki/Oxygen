package oxygen.http.schema.partial

import oxygen.http.schema.ParamType
import oxygen.schema.AnySchema

enum PartiallyAppliedPathSchema {
  case Const(path: String)
  case Param(name: String, tpe: ParamType.Path, schema: AnySchema, doc: Option[String])
}
