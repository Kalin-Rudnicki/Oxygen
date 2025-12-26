package oxygen.sql.generic.model

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

enum AggType {
  case Required
  case Optional
  case Many[S[_]](sType: Type[S], sTypeRepr: TypeRepr, seqOpsExpr: Expr[SeqOps[S]])
  case ManyNonEmpty

  final def show: String = this match
    case AggType.Required              => "Required"
    case AggType.Optional              => "Optional"
    case AggType.Many(_, sTypeRepr, _) => s"Many[${sTypeRepr.showAnsiCode}]"
    case AggType.ManyNonEmpty          => "ManyNonEmpty"

}
