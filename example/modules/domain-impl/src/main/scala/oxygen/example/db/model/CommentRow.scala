package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.query.dsl.compile
import oxygen.sql.schema.*

@tableName("comment")
final case class CommentRow(
    @primaryKey id: CommentId,
    postId: PostId,
    userId: UserId,
    comment: String,
    createdAt: Instant,
)
object CommentRow extends TableCompanion[CommentRow, CommentId](TableRepr.derived[CommentRow]) {

  @compile
  val commentsForPost: QueryIO[PostId, CommentRow] =
    for {
      postId <- input[PostId]
      c <- select[CommentRow]
      _ <- where if c.postId == postId
    } yield c

}
