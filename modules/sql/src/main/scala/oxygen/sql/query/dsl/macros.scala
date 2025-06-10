package oxygen.sql.query.dsl

import oxygen.sql.generic.Macros
import oxygen.sql.query.*
import scala.quoted.*

private[dsl] object macros {

  def selectNoInput[O: Type](select: Expr[SelectNoInput], f: Expr[Unit => Returning[O]])(using quotes: Quotes): Expr[QueryO[O]] =
    (new Macros).selectNoInputs(select, f)

  def selectInput[I: Type, O: Type](select: Expr[SelectInput[I]], f: Expr[I => Returning[O]])(using quotes: Quotes): Expr[QueryIO[I, O]] =
    (new Macros).selectInputs(select, f)

}
