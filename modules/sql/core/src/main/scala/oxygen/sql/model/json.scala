package oxygen.sql.model

final case class Json(value: String)

final case class Jsonb(value: String)

final case class TypedJson[A](value: A)

final case class TypedJsonb[A](value: A)
