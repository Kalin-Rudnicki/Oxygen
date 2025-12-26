package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

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
