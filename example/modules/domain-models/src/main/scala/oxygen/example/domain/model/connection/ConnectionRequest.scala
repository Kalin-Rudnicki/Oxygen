package oxygen.example.domain.model.connection

import java.time.Instant
import oxygen.example.core.model.*

final case class ConnectionRequest(
    current: UserId,
    other: UserId,
    createdAt: Instant,
)
