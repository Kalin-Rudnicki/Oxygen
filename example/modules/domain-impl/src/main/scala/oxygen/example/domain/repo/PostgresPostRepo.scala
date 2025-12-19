package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.*
import oxygen.example.conversion.domainToDb.*
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.post.*
import oxygen.sql.{Connection as _, *}
import oxygen.sql.query.{PostgresCRUDRepo, TableCompanion}
import zio.*

final case class PostgresPostRepo(
    db: Database,
) extends PostRepo { self =>

  private object PostgresPosts extends Posts, PostgresCRUDRepo.MapInfallible[PostId, Post] {

    override val db: Database = self.db

    override protected type DbA = PostRow
    override protected type DbK = PostId

    override protected val companion: TableCompanion[PostRow, PostId] = PostRow

    override protected def keyToDb(key: PostId): PostId = key
    override protected def valueToDb(value: Post): PostRow = value.toDb
    override protected def valueToDomain(value: PostRow): Post = value.toDomain

    override def postsByUser(userId: UserId): UIO[Seq[Post]] =
      PostRow.postsForUser.map(_.toDomain).execute(userId).arraySeq.orDie.usingDb(db)

  }

  private object PostgresComments extends Comments, PostgresCRUDRepo.MapInfallible[CommentId, Comment] {

    override val db: Database = self.db

    override protected type DbA = CommentRow
    override protected type DbK = CommentId

    override protected val companion: TableCompanion[CommentRow, CommentId] = CommentRow

    override protected def keyToDb(key: CommentId): CommentId = key
    override protected def valueToDb(value: Comment): CommentRow = value.toDb
    override protected def valueToDomain(value: CommentRow): Comment = value.toDomain

    override def commentsForPost(postId: PostId): UIO[Seq[Comment]] =
      CommentRow.commentsForPost.map(_.toDomain).execute(postId).arraySeq.orDie.usingDb(db)

  }

  override val post: Posts = PostgresPosts
  override val comment: Comments = PostgresComments

}
object PostgresPostRepo {

  val layer: URLayer[Database, PostRepo] =
    ZLayer.fromFunction { PostgresPostRepo.apply }

}
