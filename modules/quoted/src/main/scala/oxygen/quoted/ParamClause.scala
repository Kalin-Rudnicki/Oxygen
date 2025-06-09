package oxygen.quoted

import oxygen.quoted.companion.*
import oxygen.quoted.error.UnknownCase
import scala.annotation.experimental
import scala.quoted.*

sealed trait ParamClause {
  type This <: ParamClause
  val quotes: Quotes
  val unwrap: quotes.reflect.ParamClause
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.ParamClause = unwrap.asInstanceOf[newQuotes.reflect.ParamClause]

  given givenQuotes: quotes.type = quotes

  /** List of parameters of the clause */
  def params: List[ValDef] | List[TypeDef]

}
object ParamClause {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.ParamClause): ParamClause = unwrap match
    case unwrap: quotes.reflect.TypeParamClause => TypeParamClause.wrap(unwrap)
    case unwrap: quotes.reflect.TermParamClause => TermParamClause.wrap(unwrap)
    case _                                      => throw UnknownCase("ParamClause", unwrap)

  def companion(using quotes: Quotes): ParamClauseCompanion = ParamClauseCompanion(using quotes)
  given Quotes => Conversion[ParamClause.type, ParamClauseCompanion] = _.companion

}

final class TypeParamClause(val quotes: Quotes)(val unwrap: quotes.reflect.TypeParamClause) extends ParamClause {
  override type This <: TypeParamClause
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TypeParamClause = unwrap.asInstanceOf[newQuotes.reflect.TypeParamClause]

  /** List of parameters of the clause */
  override def params: List[TypeDef] = this.unwrap.params.map(TypeDef.wrap(_))

}
object TypeParamClause {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TypeParamClause): TypeParamClause =
    new TypeParamClause(quotes)(unwrap)

  def companion(using quotes: Quotes): TypeParamClauseCompanion = TypeParamClauseCompanion(using quotes)
  given Quotes => Conversion[TypeParamClause.type, TypeParamClauseCompanion] = _.companion

  def unapply(x: TypeParamClause): Some[List[TypeDef]] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.TypeParamClause.unapply(x.unwrapWithin)
    Some(unwrap.value.map(TypeDef.wrap(_)))
  }

}

final class TermParamClause(val quotes: Quotes)(val unwrap: quotes.reflect.TermParamClause) extends ParamClause {
  override type This <: TermParamClause
  override def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.TermParamClause = unwrap.asInstanceOf[newQuotes.reflect.TermParamClause]

  /** List of parameters of the clause */
  override def params: List[ValDef] = this.unwrap.params.map(ValDef.wrap(_))

  /** Is this an implicit parameter clause `(implicit x1: X1, ..., xn: Xn)` */
  def isImplicit: Boolean = this.unwrap.isImplicit

  /** Is this a given parameter clause `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)` */
  def isGiven: Boolean = this.unwrap.isGiven

  /** List of `erased` flags for each parameter of the clause */
  @experimental
  def erasedArgs: List[Boolean] = this.unwrap.erasedArgs

  /** Whether the clause has any erased parameters */
  @experimental
  def hasErasedArgs: Boolean = this.unwrap.hasErasedArgs

}
object TermParamClause {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.TermParamClause): TermParamClause =
    new TermParamClause(quotes)(unwrap)

  def companion(using quotes: Quotes): TermParamClauseCompanion = TermParamClauseCompanion(using quotes)
  given Quotes => Conversion[TermParamClause.type, TermParamClauseCompanion] = _.companion

  def unapply(x: TermParamClause): Some[List[ValDef]] = {
    given q: Quotes = x.quotes
    val unwrap = q.reflect.TermParamClause.unapply(x.unwrapWithin)
    Some(unwrap.value.map(ValDef.wrap(_)))
  }

}
