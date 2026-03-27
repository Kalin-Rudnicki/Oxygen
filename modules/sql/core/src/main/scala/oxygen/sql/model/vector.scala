package oxygen.sql.model

import oxygen.predef.core.*

sealed trait PgVectorLike

final case class PgVector(vector: ArraySeq[Float]) extends PgVectorLike

final case class PgVectorSized[I <: Int](vector: ArraySeq[Float]) extends PgVectorLike
