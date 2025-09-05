package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.user.*
import oxygen.sql.query.*
import oxygen.sql.schema.*

@tableName("connection")
final case class ConnectionRow(
    @primaryKey currentUserId: UserId,
    @primaryKey otherUserId: UserId,
    createdAt: Instant,
)
object ConnectionRow extends TableCompanion[ConnectionRow, (UserId, UserId)](TableRepr.derived[ConnectionRow])
