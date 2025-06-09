package oxygen.quoted.companion

import oxygen.quoted.*
import scala.annotation.unused
import scala.quoted.*

final class ParamClauseCompanion(using @unused quotes: Quotes) {}

final class TypeParamClauseCompanion(using quotes: Quotes) {

  def apply(params: List[TypeDef]): TypeParamClause =
    TypeParamClause.wrap(quotes.reflect.TypeParamClause.apply(params.map(_.unwrapWithin)))

}

final class TermParamClauseCompanion(using quotes: Quotes) {

  def apply(params: List[ValDef]): TermParamClause =
    TermParamClause.wrap(quotes.reflect.TermParamClause.apply(params.map(_.unwrapWithin)))

}
