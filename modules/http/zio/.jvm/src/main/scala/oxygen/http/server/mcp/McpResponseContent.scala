package oxygen.http.server.mcp

import oxygen.json.{Json, JsonEncoder}

// TODO (KR) : support other types
sealed trait McpResponseContent {
  def toJson: Json
}
object McpResponseContent {

  def plainText(value: String): McpResponseContent = McpResponseContent.PlainText(value)
  def jsonText(value: Json): McpResponseContent = McpResponseContent.JsonText(value)
  def encodeJsonText[A: JsonEncoder as enc](value: A): McpResponseContent = McpResponseContent.JsonText(enc.encodeJsonAST(value))

  final case class PlainText(text: String) extends McpResponseContent {
    override def toJson: Json = Json.obj("type" -> Json.string("text"), "text" -> Json.string(text))
  }

  final case class JsonText(json: Json) extends McpResponseContent {
    override def toJson: Json = Json.obj("type" -> Json.string("text"), "text" -> Json.string(json.showCompact))
  }

}
