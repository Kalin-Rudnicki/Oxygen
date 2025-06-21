package oxygen.sql.query.dsl2

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.*
import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
final class compile extends MacroAnnotation {

  private def transform(definition: Definition)(using Quotes): Definition = {
    val valDef: ValDef = definition.narrow[ValDef]
    val rhs: Term = valDef.requiredRHS
    val newRHS: Term = ParseContext.root("compile") { ParsedQuery.parse(rhs) }.toTerm

    report.info(newRHS.showAnsiCode)

    val newValDef: ValDef = ValDef.companion.apply(valDef.symbol, newRHS.some)
    newValDef
  }

  override def transform(using quotes: Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] =
    transform(Definition.wrap(using quotes)(definition)).unwrapWithin :: Nil

}
