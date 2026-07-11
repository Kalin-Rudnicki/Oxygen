package oxygen.http.api

import oxygen.http.server.*
import scala.annotation.experimental

@experimental given ServerErrorHandler[ResourceApi.ApiError] = ServerErrorHandler.notHandled

@experimental given DeriveEndpoints[UIApi] = DeriveEndpoints.derived
@experimental given DeriveEndpoints[ResourceApi] = DeriveEndpoints.derived
