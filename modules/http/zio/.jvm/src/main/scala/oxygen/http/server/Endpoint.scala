package oxygen.http.server

import zio.*
import zio.http.Response

final case class Endpoint[-Api](
    schema: EndpointSchema,
    handle: Api => EndpointInput => Option[URIO[Scope, Option[Response]]],
) {

  def apply(api: Api): AppliedEndpoint =
    AppliedEndpoint(
      schema = schema,
      handle = this.handle(api),
    )

}
