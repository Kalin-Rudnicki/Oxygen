package oxygen.json.syntax

import zio.json.*
import zio.json.ast.Json

object conversion {

  extension [A](self: A) {

    /**
      * Encodes the given A to a string, and then parses it back into a Json AST.
      * If the given json encoder is somehow bricked, and does not produce a valid Json AST, the result will be a raw Json string.
      */
    def safeToJsonAST(implicit encoder: JsonEncoder[A]): Json =
      self.toJson.toJsonASTDefaultString

  }

  extension (self: String) {

    /**
      * Attempts to decode the given string into a Json AST.
      * If the given string is not valid Json, the result with be a raw Json string.
      */
    def toJsonASTDefaultString: Json =
      Json.decoder.decodeJson(self).getOrElse(Json.Str(self))

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
