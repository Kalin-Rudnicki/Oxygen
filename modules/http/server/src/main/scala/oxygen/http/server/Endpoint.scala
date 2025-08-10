package oxygen.http.server

import oxygen.http.core.PathCodec
import oxygen.http.model.*
import zio.*

final case class Endpoint(
    pathElems: List[List[PathCodec.Spec]],
    run: RequestContext => Option[URIO[Scope, HttpResponse]],
)
