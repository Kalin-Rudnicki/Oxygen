package oxygen.schema

import oxygen.predef.core.*

object instances {

  given typeTagSchema: JsonSchema[TypeTag[?]] = JsonSchema.string.transform[TypeTag[?]](_ => TypeTag[Any], _.prefixObject)
  given throwableReprSchema: JsonSchema[ThrowableRepr] = JsonSchema.derived

  object throwable {

    given encoded: JsonSchema[Throwable] = throwableReprSchema.transform(_.toThrowable, ThrowableRepr.fromThrowable)
    given string: JsonSchema[Throwable] = JsonSchema.string.transform[Throwable](new RuntimeException(_), _.safeGetMessage)

  }

}
