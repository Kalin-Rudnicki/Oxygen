package oxygen.http.server

import oxygen.http.model.internal.ReceivedRequest

final case class EndpointInput(
    request: ReceivedRequest,
    errorConfig: ServerErrorConfig,
)
