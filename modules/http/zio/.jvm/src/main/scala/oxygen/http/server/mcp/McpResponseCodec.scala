package oxygen.http.server.mcp

import oxygen.predef.core.*
import oxygen.schema.*

trait McpResponseCodec[A] {
  def encode(value: A): Growable[McpResponseContent]
}
object McpResponseCodec extends McpResponseCodecLowPriority.LowPriority1 {

  object UnitCodec extends McpResponseCodec[Unit] {
    override def encode(value: Unit): Growable[McpResponseContent] = Growable.empty
  }

  object NothingCodec extends McpResponseCodec[Nothing] {
    override def encode(value: Nothing): Growable[McpResponseContent] = Growable.empty
  }

  object ThrowableCodec extends McpResponseCodec[Throwable] {
    override def encode(value: Throwable): Growable[McpResponseContent] = Growable.single(McpResponseContent.plainText(value.safeGetMessage))
  }

  final case class FromPlainTextSchema[A](schema: PlainTextSchema[A]) extends McpResponseCodec[A] {
    override def encode(value: A): Growable[McpResponseContent] = Growable.single(McpResponseContent.plainText(schema.encode(value)))
  }

  final case class FromJsonSchema[A](schema: JsonSchema[A]) extends McpResponseCodec[A] {
    override def encode(value: A): Growable[McpResponseContent] = Growable.single(McpResponseContent.jsonText(schema.jsonEncoder.encodeJsonAST(value)))
  }

}

object McpResponseCodecLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given McpResponseCodec[Nothing] = McpResponseCodec.NothingCodec
    given McpResponseCodec[Unit] = McpResponseCodec.UnitCodec
    given McpResponseCodec[Throwable] = McpResponseCodec.ThrowableCodec

    // TODO (KR) : tuples? others?

  }

  trait LowPriority2 extends LowPriority3 {

    given [A: PlainTextSchema as s] => McpResponseCodec[A] = McpResponseCodec.FromPlainTextSchema(s)

  }

  trait LowPriority3 {

    given [A: JsonSchema as s] => McpResponseCodec[A] = McpResponseCodec.FromJsonSchema(s)

  }

}
