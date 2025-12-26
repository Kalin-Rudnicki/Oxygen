package oxygen.sql.generic.model

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

enum AggType {
  case Required
  case Optional
  case Many[S[_]](sType: Type[S], sTypeRepr: TypeRepr, seqOpsExpr: Expr[SeqOps[S]])
  case ManyNonEmpty

  final def show: String = this match
    case AggType.Required      => "Required"
    case AggType.Optional      => "Optional"
    case AggType.Many(_, _, _) => "Many"
    case AggType.ManyNonEmpty  => "ManyNonEmpty"

}
