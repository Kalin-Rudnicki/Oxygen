package oxygen.http

import oxygen.http.core.*
import oxygen.http.schema.compiled.{CompiledApiSpec, RawCompiledApiSpec}
import oxygen.http.server.DeriveEndpoints
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import scala.annotation.experimental
import zio.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Models -- two/three versions of a product and a sum
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class Thing1(id: Int, name: String) derives JsonSchema
final case class Thing2Req(id: Int, name: String, description: String) derives JsonSchema
final case class Thing2Opt(id: Int, name: String, description: Option[String]) derives JsonSchema

enum Shape2 derives JsonSchema {
  case Circle(r: Double)
  case Square(s: Double)
}
enum Shape3 derives JsonSchema {
  case Circle(r: Double)
  case Square(s: Double)
  case Triangle(b: Double)
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      APIs -- request-only and response-only carriers of each model.
//      Every version pins the SAME `@apiName` + endpoint (method) name so the diff pairs them.
//////////////////////////////////////////////////////////////////////////////////////////////////////

@apiName("reqThing") @experimental trait ReqThingV1 { @route.post("/thing") def put(@param.body.json body: Thing1): UIO[Unit] }
@apiName("reqThing") @experimental trait ReqThingV2Req { @route.post("/thing") def put(@param.body.json body: Thing2Req): UIO[Unit] }
@apiName("reqThing") @experimental trait ReqThingV2Opt { @route.post("/thing") def put(@param.body.json body: Thing2Opt): UIO[Unit] }

@apiName("respThing") @experimental trait RespThingV1 { @route.get("/thing") def get(): UIO[Thing1] }
@apiName("respThing") @experimental trait RespThingV2Req { @route.get("/thing") def get(): UIO[Thing2Req] }

@apiName("reqShape") @experimental trait ReqShapeV2 { @route.post("/shape") def put(@param.body.json body: Shape2): UIO[Unit] }
@apiName("reqShape") @experimental trait ReqShapeV3 { @route.post("/shape") def put(@param.body.json body: Shape3): UIO[Unit] }

@apiName("respShape") @experimental trait RespShapeV2 { @route.get("/shape") def get(): UIO[Shape2] }
@apiName("respShape") @experimental trait RespShapeV3 { @route.get("/shape") def get(): UIO[Shape3] }

@apiName("multi") @experimental trait MultiV1 { @route.get("/a") def a(): UIO[Thing1] }
@apiName("multi") @experimental trait MultiV2 {
  @route.get("/a") def a(): UIO[Thing1]
  @route.get("/b") def b(): UIO[Thing1]
}

@experimental
object ApiSpecCompatFixtures {

  private def specOf[A](endpoints: DeriveEndpoints[A]): RawCompiledApiSpec =
    CompiledApiSpec.compileWithoutLineNos(endpoints.endpoints.toArraySeq.map(_.schema))

  val reqThingV1: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[ReqThingV1])
  val reqThingV2Req: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[ReqThingV2Req])
  val reqThingV2Opt: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[ReqThingV2Opt])

  val respThingV1: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[RespThingV1])
  val respThingV2Req: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[RespThingV2Req])

  val reqShapeV2: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[ReqShapeV2])
  val reqShapeV3: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[ReqShapeV3])

  val respShapeV2: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[RespShapeV2])
  val respShapeV3: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[RespShapeV3])

  val multiV1: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[MultiV1])
  val multiV2: RawCompiledApiSpec = specOf(DeriveEndpoints.derived: DeriveEndpoints[MultiV2])

}
