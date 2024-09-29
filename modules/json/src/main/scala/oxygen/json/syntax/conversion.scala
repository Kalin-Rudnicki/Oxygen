package oxygen.json.syntax

import zio.json.*
import zio.json.ast.Json

object conversion {

  extension [A](self: A) {
    def safeToJsonAST(implicit encoder: JsonEncoder[A]): Json = {
      val encoded = self.toJson
      Json.decoder.decodeJson(encoded).getOrElse(Json.Str(encoded))
    }
  }

  extension (self: Json) {
    def simpleJsonString: String = self match
      case Json.Str(value) => value
      case _               => self.toJson
    def simpleJsonStringPretty: String = self match
      case Json.Str(value) => value
      case _               => self.toJsonPretty
  }

}
