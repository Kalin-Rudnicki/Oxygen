package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.post.*
import oxygen.sql.{Connection as _, *}
import zio.*

final case class PostgresPostRepo(
    db: Database,
) extends PostRepo {

  override def insertPost(post: Post): UIO[Unit] =
    PostRow.insert.execute(post.toDb).unit.orDie.usingDb(db)

  override def findPost(postId: PostId): UIO[Option[Post]] =
    PostRow.selectByPK.map(_.toDomain).execute(postId).option.orDie.usingDb(db)

  override def postsByUser(userId: UserId): UIO[Seq[Post]] =
    PostRow.postsForUser.map(_.toDomain).execute(userId).arraySeq.orDie.usingDb(db)

  override def insertComment(comment: Comment): UIO[Unit] =
    CommentRow.insert.execute(comment.toDb).unit.orDie.usingDb(db)

  override def commentsForPost(postId: PostId): UIO[Seq[Comment]] =
    CommentRow.commentsForPost.map(_.toDomain).execute(postId).arraySeq.orDie.usingDb(db)

}
object PostgresPostRepo {

  val layer: URLayer[Database, PostRepo] =
    ZLayer.fromFunction { PostgresPostRepo.apply }

}
