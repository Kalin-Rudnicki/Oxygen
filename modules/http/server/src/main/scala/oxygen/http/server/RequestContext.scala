package oxygen.http.server

import oxygen.http.model.*

final case class RequestContext(
    request: HttpRequest,
    exposeInternalErrors: Boolean,
)
