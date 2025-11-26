package oxygen.sql.generic.model.part

import oxygen.sql.generic.parsing.*

sealed trait PartialQuery
object PartialQuery extends PartialQueryParsers[PartialQuery] {

  sealed trait InsertQuery extends PartialQuery
  object InsertQuery extends PartialQueryParsers[InsertQuery] {

    final case class Basic(
        insert: InsertPart.Basic,
        into: IntoPart,
    ) extends InsertQuery
    object Basic {

      lazy val parser: MapChainParser[InsertQuery.Basic] =
        (
          InsertPart.Basic.withContext("Insert") >>>
            IntoPart.withContext("Into")
        ).withContext("Basic Insert Query").map { PartialQuery.InsertQuery.Basic.apply }

    }

    final case class FromSelect(
        insert: InsertPart.FromSelect,
        select: PartialQuery.SelectQuery,
        into: IntoPart.FromSelect,
    ) extends InsertQuery
    object FromSelect {

      lazy val parser: MapChainParser[InsertQuery.FromSelect] =
        (
          InsertPart.FromSelect.withContext("Insert") >>>
            PartialQuery.SelectQuery.partialParser >>>
            IntoPart.FromSelect.withContext("Into")
        ).withContext("Insert From Select Query").map { PartialQuery.InsertQuery.FromSelect.apply }

    }

    override lazy val partialParser: MapChainParser[InsertQuery] =
      Basic.parser || FromSelect.parser

  }

  final case class SelectQuery(
      select: SelectPart,
      joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
  ) extends PartialQuery
  object SelectQuery extends PartialQueryParsers[SelectQuery] {

    override lazy val partialParser: MapChainParser[SelectQuery] =
      (
        SelectPart.withContext("Select") >>>
          JoinPart.many.withContext("Join") >>>
          WherePart.maybe.withContext("Where") >>>
          OrderByPart.maybe.withContext("OrderBy") >>>
          LimitPart.maybe.withContext("Limit") >>>
          OffsetPart.maybe.withContext("Offset")
      ).withContext("Select Query").map { PartialQuery.SelectQuery.apply }

  }

  final case class UpdateQuery(
      update: UpdatePart,
      joins: List[JoinPart],
      where: Option[WherePart],
      set: SetPart,
  ) extends PartialQuery
  object UpdateQuery extends PartialQueryParsers[UpdateQuery] {

    override lazy val partialParser: MapChainParser[UpdateQuery] =
      (
        UpdatePart.withContext("Update") >>>
          JoinPart.many.withContext("Join") >>>
          WherePart.maybe.withContext("Where") >>>
          SetPart.withContext("Set")
      ).withContext("Update Query").map { PartialQuery.UpdateQuery.apply }

  }

  final case class DeleteQuery(
      delete: DeletePart,
      joins: List[JoinPart],
      where: Option[WherePart],
  ) extends PartialQuery
  object DeleteQuery extends PartialQueryParsers[DeleteQuery] {

    override lazy val partialParser: MapChainParser[DeleteQuery] =
      (
        DeletePart.withContext("Delete") >>>
          JoinPart.many.withContext("Join") >>>
          WherePart.maybe.withContext("Where")
      ).withContext("Delete Query").map { PartialQuery.DeleteQuery.apply }

  }

  override lazy val partialParser: MapChainParser[PartialQuery] =
    InsertQuery.partialParser || SelectQuery.partialParser || UpdateQuery.partialParser || DeleteQuery.partialParser

}
