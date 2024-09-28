package oxygen.predef

object json {
  export oxygen.json.{EncodedThrowable, InJsonString}
  export oxygen.json.instances.*
  export oxygen.json.syntax.instances.*
  export zio.json.{DecoderOps, EncoderOps}
  export zio.json.{JsonCodec, JsonDecoder, JsonEncoder}
}
