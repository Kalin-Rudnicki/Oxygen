package oxygen.schema

type AnySchemaT[A] = PlainTextSchema[A] | JsonSchema[A]
type AnySchema = PlainTextSchema[?] | JsonSchema[?]
