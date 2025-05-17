package oxygen.sql.query.dsl

import oxygen.predef.meta.*
import oxygen.sql.generic.Macros
import oxygen.sql.query.*

private[dsl] object macros {

  def selectNoInput[O: Type](select: Expr[SelectNoInput], f: Expr[Unit => Returning[O]])(using quotes: Quotes): Expr[QueryO[O]] =
    Macros(K0(Meta(quotes))).selectNoInputs(select, f)

  def selectInput[I: Type, O: Type](select: Expr[SelectInput[I]], f: Expr[I => Returning[O]])(using quotes: Quotes): Expr[QueryIO[I, O]] =
    Macros(K0(Meta(quotes))).selectInputs(select, f)

}
