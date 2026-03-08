package oxygen.sql.schema

import oxygen.predef.core.*

final case class ForeignKeyRepr[Self, References](
    explicitName: Option[String],
    references: Lazy[TableRepr[References]],
    selfColumns: TableRepr[Self] => ArraySeq[Column],
    referencesColumns: TableRepr[References] => ArraySeq[Column],
) {

  def built(self: TableRepr[Self]): ForeignKeyRepr.Built =
    ForeignKeyRepr.Built(
      explicitName,
      self,
      Lazy { references.value },
      Lazy { selfColumns(self).zipExact(referencesColumns(references.value)) },
    )

}
object ForeignKeyRepr {

  final case class Built private[ForeignKeyRepr] (
      explicitName: Option[String],
      self: TableRepr[?],
      references: Lazy[TableRepr[?]],
      columnPairs: Lazy[ArraySeq[(self: Column, references: Column)]],
  )

}
