package oxygen.example.webServer.mcp

import oxygen.http.core.*
import oxygen.http.server.{DeriveEndpoints, ServerErrorHandler}
import oxygen.predef.core.*
import oxygen.schema.{doc, JsonSchema}
import zio.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Models
//////////////////////////////////////////////////////////////////////////////////////////////////////

// `@doc` on a field (a "nested param") becomes that property's `description` in the JSON Schema.
final case class Note(
    @doc("The note's server-assigned id, e.g. \"note-1\".") id: String,
    @doc("Short, human-readable title.") title: String,
    @doc("Free-form body text.") body: String,
) derives JsonSchema
final case class CreateNote(
    @doc("Short, human-readable title.") title: String,
    @doc("Free-form body text.") body: String,
) derives JsonSchema

enum NoteError derives JsonSchema, StatusCodes {
  @statusCode.`404` case NotFound(id: String)
}
object NoteError {
  given ServerErrorHandler[NoteError] = ServerErrorHandler.notHandled
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      API
//////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * A tiny, in-memory, auth-less example API. Every route is opted in to MCP with `@mcp.tool`, so the
  * same trait is served as a normal HTTP API *and* as MCP tools (via the `EndpointMiddlewares` builder
  * in `WebServerMain`). No `@mcp.auth` anywhere — `McpAuthService.NoAuth`.
  */
trait NoteApi derives DeriveEndpoints {

  // Docs flow into the MCP tool at three levels:
  //   - `@httpDoc` on the route  -> the tool's `description`
  //   - `@httpDoc` on a param    -> that input property's `description` (a "flat param")
  //   - `@doc` on a model field  -> that property's `description` inside `$defs` (a "nested param")

  @mcp.tool
  @httpDoc("Create a note and return it (with its generated id).")
  @route.post("/api/notes")
  def create(@httpDoc("The note to create.") @param.body req: CreateNote): IO[NoteError, Note]

  @mcp.tool
  @httpDoc("Fetch a single note by id.")
  @route.get("/api/notes/%")
  def get(@httpDoc("The id of the note to fetch, e.g. \"note-1\".") @param.path id: String): IO[NoteError, Note]

  @mcp.tool
  @httpDoc("List all notes.")
  @route.get("/api/notes")
  def list(): IO[NoteError, List[Note]]

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Impl
//////////////////////////////////////////////////////////////////////////////////////////////////////

/** In-memory implementation backed by `Ref`s — no database. */
final case class NoteApiImpl(notes: Ref[Map[String, Note]], nextId: Ref[Long]) extends NoteApi {

  override def create(req: CreateNote): IO[NoteError, Note] =
    for {
      id <- nextId.modify(n => (s"note-${n + 1}", n + 1))
      note = Note(id, req.title, req.body)
      _ <- notes.update(_.updated(id, note))
    } yield note

  override def get(id: String): IO[NoteError, Note] =
    notes.get.map(_.get(id)).someOrFail(NoteError.NotFound(id))

  override def list(): IO[NoteError, List[Note]] =
    notes.get.map(_.values.toList.sortBy(_.id))

}
object NoteApiImpl {

  val layer: ULayer[NoteApi] =
    ZLayer.fromZIO {
      for {
        notes <- Ref.make(Map.empty[String, Note])
        nextId <- Ref.make(0L)
      } yield NoteApiImpl(notes, nextId)
    }

}
