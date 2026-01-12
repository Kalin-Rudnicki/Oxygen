package oxygen.predef

object json {
  export oxygen.json.{Json, JsonCodec, JsonDecoder, JsonEncoder, JsonError, KeyedMapDecoder}
  export oxygen.json.{jsonDiscriminator, jsonField, jsonFlatten, jsonType}
  export oxygen.json.instances.given
  export oxygen.json.syntax.build.*
  export oxygen.json.syntax.json.*
}
