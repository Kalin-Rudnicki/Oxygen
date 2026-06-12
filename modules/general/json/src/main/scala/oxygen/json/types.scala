package oxygen.json

import oxygen.predef.core.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      PlainTextJson
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type PlainTextJson <: Json = Json
object PlainTextJson {

  def wrap(json: Json): PlainTextJson = json

  val Null: PlainTextJson = Json.Null

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      PlainTextJsonObject
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type PlainTextJsonObject <: Json.Obj & PlainTextJson = Json.Obj & PlainTextJson
object PlainTextJsonObject {

  def wrap(json: Json.Obj): PlainTextJsonObject = json
  def apply(fields: ArraySeq[(String, PlainTextJson)]): PlainTextJsonObject = Json.Obj(fields)

  extension (self: PlainTextJsonObject)
    inline def plainTextValue: ArraySeq[(String, PlainTextJson)] = self.value
    inline def plainTextValueMap: Map[String, PlainTextJson] = self.valueMap

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      PlainTextJsonArray
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type PlainTextJsonArray <: Json.Arr & PlainTextJson = Json.Arr & PlainTextJson
object PlainTextJsonArray {

  // def wrap(json: Json.Obj): PlainTextJsonObject = json
  def apply(fields: ArraySeq[PlainTextJson]): PlainTextJsonArray = Json.Arr(fields)

  extension (self: PlainTextJsonArray)
    inline def plainTextValue: ArraySeq[PlainTextJson] = self.value

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SecretJson
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type SecretJson <: Json = Json
object SecretJson {

  def wrap(json: Json): SecretJson = json

  val Null: SecretJson = Json.Null

  /**
    * Accepts a json that could be either [[PlainTextJson]] and/or [[SecretJson]], and makes it all [[SecretJson]].
    */
  def iorToSecret(ior: Ior[PlainTextJson, SecretJson]): SecretJson =
    ior match
      case Ior.Left(left)        => left
      case Ior.Right(right)      => right
      case Ior.Both(left, right) => left merge right

  /**
    * Accepts a json that could be either [[PlainTextJson]] and/or [[SecretJson]], and makes it all [[SecretJson]].
    * Then wraps it back in an [[Ior.Right]] to conform with the original type.
    */
  def iorToRight(ior: Ior[PlainTextJson, SecretJson]): Ior.Right[SecretJson] =
    Ior.Right(iorToSecret(ior))

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SecretJsonObject
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type SecretJsonObject <: Json.Obj & SecretJson = Json.Obj & SecretJson
object SecretJsonObject {

  def wrap(json: Json.Obj): SecretJsonObject = json
  def apply(fields: ArraySeq[(String, SecretJson)]): SecretJsonObject = Json.Obj(fields)

  extension (self: SecretJsonObject)
    inline def secretValue: ArraySeq[(String, SecretJson)] = self.value
    inline def secretValueMap: Map[String, SecretJson] = self.valueMap

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SecretJsonArray
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type SecretJsonArray <: Json.Arr & SecretJson = Json.Arr & SecretJson
object SecretJsonArray {

  // def wrap(json: Json.Obj): SecretJsonArray = json
  def apply(fields: ArraySeq[SecretJson]): SecretJsonArray = Json.Arr(fields)

  extension (self: SecretJsonArray)
    inline def secretValue: ArraySeq[SecretJson] = self.value

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Strings
//////////////////////////////////////////////////////////////////////////////////////////////////////

opaque type PlainTextJsonFormattedString <: String = String
object PlainTextJsonFormattedString {
  def wrap(json: String): PlainTextJsonFormattedString = json
}

opaque type SecretJsonFormattedString <: String = String
object SecretJsonFormattedString {
  def wrap(json: String): SecretJsonFormattedString = json
}
