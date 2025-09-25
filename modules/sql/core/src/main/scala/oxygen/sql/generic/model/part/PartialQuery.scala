package oxygen.sql.generic.model.part

import oxygen.sql.generic.parsing.*

sealed trait PartialQuery
object PartialQuery extends PartialQueryParsers[PartialQuery] {

  final case class InsertQuery(
      insert: InsertPart,
      into: IntoPart,
  ) extends PartialQuery
  object InsertQuery extends PartialQueryParsers[InsertQuery] {

    override lazy val partialParser: MapChainParser[InsertQuery] =
      (
        InsertPart.withContext("Insert") >>>
          IntoPart.withContext("Into")
      ).withContext("Insert Query").map { PartialQuery.InsertQuery.apply }

  }

  final case class SelectQuery(
      select: SelectPart,
      joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
  ) extends PartialQuery
  object SelectQuery extends PartialQueryParsers[SelectQuery] {

    override lazy val partialParser: MapChainParser[SelectQuery] =
      (
        SelectPart.withContext("Select") >>>
          JoinPart.many.withContext("Join") >>>
          WherePart.maybe.withContext("Where") >>>
          OrderByPart.maybe.withContext("OrderBy") >>>
          LimitPart.maybe.withContext("Limit")
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
