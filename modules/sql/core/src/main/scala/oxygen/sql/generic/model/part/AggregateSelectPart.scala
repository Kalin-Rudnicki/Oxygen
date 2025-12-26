package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

sealed trait AggregateSelectPart
object AggregateSelectPart {

  final case class ReturningLeaf(
      // TODO (KR) : are inputs needed, or a separate type?
      //           : inputs: List[InputPart],
      select: SelectPart,
      // TODO (KR) : maybe support joins?
      //           : joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      retExpr: QueryExpr.QueryVariableReferenceLike.ReferencedVariable,
      refs: RefMap,
  ) extends AggregateSelectPart

  final case class ReturningNested(
      // TODO (KR) : are inputs needed, or a separate type?
      //           : inputs: List[InputPart],
      select: SelectPart,
      // TODO (KR) : maybe support joins?
      //           : joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      ret: NonEmptyList[AggregateElemPart],
      refs: RefMap,
  ) extends AggregateSelectPart

}
