package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*

sealed trait AggregateElemPart
object AggregateElemPart {

  final case class ReturnSelf(retExpr: QueryExpr.QueryVariableReferenceLike.ReferencedVariable) extends AggregateElemPart

  final case class ReturnAggregate(
      aggType: AggType,
      select: AggregateSelectPart,
      aType: TypeRepr,
      outType: TypeRepr,
  ) extends AggregateElemPart

}
