package oxygen.http.server

import zio.http.Request

final case class EndpointInput(
    request: Request,
    exposeInternalErrors: Boolean,
)
