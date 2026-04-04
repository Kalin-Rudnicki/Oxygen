package oxygen.sql.model

import oxygen.predef.core.*

final case class LTree(labels: ArraySeq[String]) {
  def :+(append: String): LTree = LTree(labels :+ append)
  def ++(append: IterableOnce[String]): LTree = LTree(labels ++ append)
}
