package oxygen.sql.schema

import oxygen.predef.core.*

final case class IndexRepr[Self](
    explicitName: Option[String],
    unique: Boolean,
    columns: TableRepr[Self] => ArraySeq[Column],
) {

  def built(self: TableRepr[Self]): IndexRepr.Built =
    IndexRepr.Built(
      explicitName,
      unique,
      self,
      Lazy { columns(self) },
    )

}
object IndexRepr {

  final case class Built private[IndexRepr] (
      explicitName: Option[String],
      unique: Boolean,
      self: TableRepr[?],
      columns: Lazy[ArraySeq[Column]],
  )

}
