package oxygen.schema

import oxygen.crypto.model.JWT
import oxygen.json.JsonCodec
import oxygen.predef.core.*

object instances {

  given typeTagSchema: JsonSchema[TypeTag[? <: AnyKind]] = JsonSchema.string.transform[TypeTag[? <: AnyKind]](_ => TypeTag[Any], _.prefixObject)
  given throwableReprSchema: JsonSchema[ThrowableRepr] = JsonSchema.derived

  given standardJWTPayloadSchema: [A: JsonSchema] => (typeTag: TypeTag[JWT.StandardPayload[A]]) => JsonSchema[JWT.StandardPayload[A]] = JsonSchema.derived[JWT.StandardPayload[A]]

  given stringCodecFromSchema: [A: PlainTextSchema as schema] => StringCodec[A] = StringCodec(schema.encode(_), StringDecoder.string.mapOrFail(schema.decode)(using schema.typeTag))
  given jsonCodecFromSchema: [A: JsonSchema as schema] => JsonCodec[A] = JsonCodec(schema.jsonEncoder, schema.jsonDecoder)

  object throwable {

    given encoded: JsonSchema[Throwable] = throwableReprSchema.transform(_.toThrowable, ThrowableRepr.fromThrowable)
    given string: JsonSchema[Throwable] = JsonSchema.string.transform[Throwable](new RuntimeException(_), _.safeGetMessage)

  }

}
