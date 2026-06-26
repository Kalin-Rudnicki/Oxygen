package oxygen.http.server

import oxygen.http.server.mcp.McpEndpoint
import zio.*
import zio.http.Response

final case class Endpoint[-Api](
    schema: EndpointSchema,
    handle: Api => EndpointInput => Option[URIO[Scope, Option[Response]]],
    mcp: Option[McpEndpoint[Api]],
) {

  def apply(api: Api): AppliedEndpoint =
    AppliedEndpoint(
      schema = schema,
      handle = this.handle(api),
      mcp = mcp.map(_(api)),
    )

}
