package oxygen.example.domain.model.connection

import java.time.Instant
import oxygen.example.core.model.*

final case class Connection(
    current: UserId,
    other: UserId,
    createdAt: Instant,
)
