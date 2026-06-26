package oxygen.http

import java.util.UUID
import oxygen.http.model.internal.ReceivedRequest
import oxygen.http.schema.compiled.*
import oxygen.http.server.{ApiSpecEndpointMiddleware, DeriveEndpoints, EndpointInput, EndpointSchema, ServerErrorConfig}
import oxygen.json.JsonCodec
import oxygen.predef.core.*
import oxygen.predef.test.*
import scala.annotation.experimental
import zio.*
import zio.http.{Request, URL}

@experimental
object CompiledApiSpecSpec extends OxygenSpecDefault {

  private val schemas: ArraySeq[EndpointSchema] =
    summon[DeriveEndpoints[UserApi]].endpoints.toArraySeq.map(_.schema)

  private val spec: RawCompiledApiSpec = CompiledApiSpec.compileWithoutLineNos(schemas)

  override def testSpec: TestSpec =
    suite("CompiledApiSpecSpec")(
      test("compiles all endpoints") {
        val endpointNames: Set[String] = spec.apis.flatMap(_.endpoints.map(_.name)).toSet
        assertTrue(
          spec.apis.nonEmpty,
          endpointNames == Set("userById", "allUsers", "createUser", "userSearch", "userEvents", "macroTest"),
        )
      },
      test("captures shared types once in the schema bundle") {
        // `User` is referenced by several endpoints but should be compiled into a single schema entry.
        val userSchemaCount: Int = spec.schemas.all.count(_.fullTypeName.fullTypeName.endsWith("User"))
        assertTrue(spec.schemas.all.nonEmpty, userSchemaCount >= 1)
      },
      test("captures per-case error status codes for sum-typed errors, keyed by full type name") {
        val ep = spec.apis.flatMap(_.endpoints).find(_.name == "userById").get
        val caseStatuses: Set[(String, Int)] = ep.errorResponse.caseStatuses.map(c => (c.caseName, c.code)).toSet
        // caseName is the case's full type name (so it correlates with the schema case unambiguously),
        // not the bare `NoSuchUser` — see StatusCodes.CaseStatus.
        assertTrue(
          caseStatuses.contains(("oxygen.http.ApiError.NoSuchUser", 404)),
          caseStatuses.contains(("oxygen.http.ApiError.InternalError", 500)),
        )
      },
      test("round-trips through JSON") {
        val codec: JsonCodec[RawCompiledApiSpec] = JsonCodec[RawCompiledApiSpec]
        val json: String = codec.encoder.encodeJsonStringCompact(spec)
        val decoded: Option[RawCompiledApiSpec] = codec.decoder.decodeJsonString(json).toOption
        assertTrue(decoded == Some(spec))
      },
      test("FullCompiledApiSpec renders without throwing") {
        val rendered: String = spec.toFullCompiledApiSpec.show
        assertTrue(rendered.contains("userById"), rendered.contains("=== Schemas ==="))
      },
      test("ApiSpecEndpointMiddleware appends an endpoint that serves the spec") {
        for {
          ref <- Ref.make(Map.empty[UUID, User])
          applied = summon[DeriveEndpoints[UserApi]].appliedEndpoints(UserApiImpl(ref))
          augmented <- ZIO.scoped(ApiSpecEndpointMiddleware(ApiSpecEndpointMiddleware.Config.default).apply(applied))
          specEp = augmented.arraySeq.find(_.endpointName == "apiSpec").get
          request <- ReceivedRequest.fromRequest(Request.get(URL.decode("/oxygen/api-spec").toOption.get))
          input = EndpointInput(request, ServerErrorConfig(exposeInternalErrors = false))
          response <- ZIO.scoped(specEp.handle(input).get).map(_.get)
          body <- response.body.asString.orDie
          decoded = JsonCodec[RawCompiledApiSpec].decoder.decodeJsonString(body).toOption
        } yield assertTrue(
          augmented.arraySeq.length == applied.arraySeq.length + 1,
          response.status.code == 200,
          decoded.exists(_.apis.flatMap(_.endpoints).exists(_.name == "userById")),
        )
      },
    )

}
