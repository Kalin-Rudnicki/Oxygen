package oxygen.mcp.domain

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*
import oxygen.schema.JsonSchema
import zio.*

/**
  * The result codec of a derived MCP tool — the [[oxygen.mcp.generic.McpDerive]] analog of oxygen-http's
  * `ResponseCodec`. A tool's success value `A` becomes an [[McpToolResult]]; a typed error `E` is forced
  * into a protocol [[McpError]]:
  *
  *   - [[Success]] — encode `A` into a successful result (`isError = false`).
  *   - [[Failure]] — map an arbitrary consumer error `E` into an [[McpError]] (the protocol error), which
  *     the HTTP layer surfaces (e.g. an auth error -> [[McpError.Unauthorized]] -> `401` + `WWW-Authenticate`).
  *
  * The default `Success` reuses a value's `JsonSchema`: the json becomes a human-readable `text` content
  * block, and (for an object) is mirrored into `structuredContent`. The default `Failure` maps `E` to a
  * generic [[McpError.InternalError]] carrying the compact-encoded value — a consumer overrides it (for
  * auth/etc.) to return [[McpError.Unauthorized]] / [[McpError.Forbidden]] / [[McpError.InvalidParams]] / …
  * A `given Failure[Nothing]` covers a tool that cannot fail typed-ly (`URIO[Scope, A]`), mirroring
  * `ResponseCodec`'s `given nothing`.
  */
object McpResponseSchema {

  /** Encode a tool's success value into a (non-error) [[McpToolResult]]. */
  trait Success[A] {
    def encode(a: A): McpToolResult
  }
  object Success {

    inline def apply[A](using ev: Success[A]): Success[A] = ev

    given [A: JsonSchema as schema] => Success[A] =
      a => {
        val json: Json = schema.jsonEncoder.encodeJsonAST(a)
        val structured: Option[API.response.StructuredContent] = json match {
          case obj: Json.Obj => Some(API.response.StructuredContent.wrap(obj))
          case _             => None
        }
        McpToolResult(API.response.ContentBlock.Text(textOf(json), None) :: Nil, structured, isError = false)
      }

  }

  /** Map a tool's typed error into the protocol [[McpError]] that surfaces at the transport. */
  trait Failure[E] {
    def toError(e: E): McpError
  }
  object Failure {

    inline def apply[E](using ev: Failure[E]): Failure[E] = ev

    /** The generic default: any `E` becomes an [[McpError.InternalError]] carrying its compact json. */
    given [E: JsonSchema as schema] => Failure[E] =
      e => McpError.InternalError(Some(schema.jsonEncoder.encodeJsonAST(e).showCompact))

    /** A tool with no typed error (`URIO[Scope, A]`); the effect can never fail, so this is unreachable. */
    given Failure[Nothing] = _ => McpError.InternalError(Some("unreachable: tool has no typed error"))

  }

  /**
    * Run a derived tool's effect into an [[McpToolResult]] — a typed failure `E` becomes a protocol
    * [[McpError]] via [[Failure]] (which then surfaces at the HTTP layer); a defect is caught (never dies)
    * and reported as an [[McpError.InternalError]]; a success becomes a result via [[Success]]. Emitted by
    * [[oxygen.mcp.generic.McpDerive]].
    */
  def run[E, A](effect: ZIO[Scope, E, A], success: Success[A], failure: Failure[E]): ZIO[Scope, McpError, McpToolResult] =
    effect.foldCauseZIO(
      cause =>
        cause.failureOption match {
          case Some(e) => ZIO.fail(failure.toError(e))
          case None    => ZIO.fail(defectError(cause))
        },
      a => ZIO.succeed(success.encode(a)),
    )

  private def defectError(cause: Cause[Any]): McpError =
    McpError.InternalError(Some(cause.failureOrCause.fold(_.toString, _.prettyPrint)))

  /** A `Json.Str` renders as its raw text; anything else as compact json. */
  private def textOf(json: Json): String =
    json match {
      case Json.Str(s) => s
      case other       => other.showCompact
    }

}
