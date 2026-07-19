package oxygen.example.domain.repo

import oxygen.example.conversion.dbToDomain.{given, *}
import oxygen.example.conversion.domainToDb.given
import oxygen.example.core.model.post.*
import oxygen.example.core.model.user.*
import oxygen.example.db.model.*
import oxygen.example.domain.model.post.*
import oxygen.sql.{Connection as _, *}
import oxygen.sql.query.PostgresCRUDRepo
import zio.*

final case class PostgresPostRepo(
    db: Database,
) extends PostRepo { self =>

  private object PostgresPosts extends Posts, PostgresCRUDRepo.TransformInfallible[PostId, Post, PostRow](PostRow) {

    override val db: Database = self.db

    override def postsByUser(userId: UserId): UIO[Seq[Post]] =
      PostRow.postsForUser.map(_.toDomain).execute(userId).arraySeq.orDie.usingDb(db)

  }

  private object PostgresComments extends Comments, PostgresCRUDRepo.TransformInfallible[CommentId, Comment, CommentRow](CommentRow) {

    override val db: Database = self.db

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
