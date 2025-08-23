package oxygen.predef

object json {
  export oxygen.json.{Json, JsonCodec, JsonDecoder, JsonEncoder, JsonError, KeyedMapDecoder}
  export oxygen.json.instances.given
  export oxygen.json.instances.throwable.encoded.given
  export oxygen.json.syntax.json.*
}
