package oxygen.http.server

import oxygen.predef.core.*
import zio.*

final case class AppliedEndpoints(endpoints: Growable[AppliedEndpoint]) {

  lazy val arraySeq: ArraySeq[AppliedEndpoint] = endpoints.toArraySeq
  def ++(that: AppliedEndpoints): AppliedEndpoints = AppliedEndpoints(this.endpoints ++ that.endpoints)

  def compile(
      requestMiddleware: RequestMiddleware,
      responseMiddleware: ResponseMiddleware,
      endpointMiddleware: EndpointMiddleware,
  ): URIO[Scope, CompiledEndpoints] =
    endpointMiddleware(this).map { finalEndpoints =>
      CompiledEndpoints.SeqScan(finalEndpoints).withMiddleware(requestMiddleware, responseMiddleware)
    }

}
object AppliedEndpoints {

  def flatten(endpoints: AppliedEndpoints*): AppliedEndpoints =
    AppliedEndpoints(Growable.many(endpoints).flatMap(_.endpoints))

  val empty: AppliedEndpoints = AppliedEndpoints(Growable.empty)

}
