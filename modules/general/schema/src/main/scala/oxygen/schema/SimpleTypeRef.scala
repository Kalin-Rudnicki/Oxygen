package oxygen.schema

import oxygen.core.TypeTag

enum SimpleTypeRef {
  case Plain(typeTag: TypeTag[?])
  case Json(typeTag: TypeTag[?])

  val typeTag: TypeTag[?]
}
