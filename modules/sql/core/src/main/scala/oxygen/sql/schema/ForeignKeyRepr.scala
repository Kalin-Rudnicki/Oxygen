package oxygen.sql.schema

import oxygen.predef.core.*

final case class ForeignKeyRepr[Self, References](
    explicitName: Option[String],
    references: TableRepr[References],
    selfColumns: TableRepr[Self] => ArraySeq[Column],
    referencesColumns: TableRepr[References] => ArraySeq[Column],
) {

  def built(self: TableRepr[Self]): ForeignKeyRepr.Built =
    ForeignKeyRepr.Built(explicitName, self, references, selfColumns(self).zipExact(referencesColumns(references)))

}
object ForeignKeyRepr {

  final case class Built private[ForeignKeyRepr] (
      explicitName: Option[String],
      self: TableRepr[?],
      references: TableRepr[?],
      columnPairs: ArraySeq[(self: Column, references: Column)],
  )

}
