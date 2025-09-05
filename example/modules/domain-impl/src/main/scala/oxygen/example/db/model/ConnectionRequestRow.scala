package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.user.*
import oxygen.sql.query.*
import oxygen.sql.schema.*

@tableName("connection_request")
final case class ConnectionRequestRow(
    @primaryKey currentUserId: UserId,
    @primaryKey otherUserId: UserId,
    createdAt: Instant,
)
object ConnectionRequestRow extends TableCompanion[ConnectionRequestRow, (UserId, UserId)](TableRepr.derived[ConnectionRequestRow])
