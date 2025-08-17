package oxygen.schema

import oxygen.core.TypeTag

enum TypeRef {
  case Plain(typeTag: TypeTag[?])
  case Json(typeTag: TypeTag[?])

  val typeTag: TypeTag[?]
}
