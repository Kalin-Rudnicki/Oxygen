package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.query.dsl.compile
import oxygen.sql.schema.*

@tableName("post")
final case class PostRow(
    @primaryKey id: PostId,
    userId: UserId,
    title: String,
    body: String,
    createdAt: Instant,
)
object PostRow extends TableCompanion[PostRow, PostId](TableRepr.derived[PostRow]) {

  @compile
  val postsForUser: QueryIO[UserId, PostRow] =
    for {
      userId <- input[UserId]
      p <- select[PostRow]
      _ <- where if p.userId == userId
    } yield p

}
