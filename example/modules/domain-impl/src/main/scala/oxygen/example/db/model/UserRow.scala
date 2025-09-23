package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.user.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.query.dsl.compile
import oxygen.sql.schema.*

@tableName("user")
final case class UserRow(
    @primaryKey @columnName("user_id") id: UserId,
    email: Email,
    referenceEmail: Email,
    firstName: String,
    lastName: String,
    hashedPassword: String,
    createdAt: Instant,
)
object UserRow extends TableCompanion[UserRow, UserId](TableRepr.derived[UserRow]) {

  @compile
  val userByEmail: QueryIO[Email, UserRow] =
    for {
      email <- input[Email]
      u <- select[UserRow]
      _ <- where if u.referenceEmail == email
    } yield u

  @compile
  val getConnections: QueryIO[UserId, UserRow] =
    for {
      userId <- input[UserId]
      c <- select[ConnectionRow]
      u <- join[UserRow] if u.id == c.otherUserId
      _ <- where if c.currentUserId == userId
    } yield u

}
