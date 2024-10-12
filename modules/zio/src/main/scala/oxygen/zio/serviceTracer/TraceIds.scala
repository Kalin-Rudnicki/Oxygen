package oxygen.zio.serviceTracer

import java.util.UUID
import zio.json.JsonCodec

final case class TraceIds(
    id: UUID,
    rootId: UUID,
    parentId: Option[UUID],
) derives JsonCodec
