package oxygen.http.server

import oxygen.predef.core.*

trait EndpointMiddleware {
  // TODO (KR) : this should probably be made effectful
  def map(endpoints: ArraySeq[Endpoint]): Growable[Endpoint]
  final def apply(endpoints: Endpoints): Endpoints = Endpoints(map(endpoints.endpoints.toArraySeq))
}
object EndpointMiddleware {

  trait Add extends EndpointMiddleware {
    def add(endpoints: ArraySeq[Endpoint]): Growable[Endpoint]
    override final def map(endpoints: ArraySeq[Endpoint]): Growable[Endpoint] = Growable.many(endpoints) ++ add(endpoints)
  }

}
