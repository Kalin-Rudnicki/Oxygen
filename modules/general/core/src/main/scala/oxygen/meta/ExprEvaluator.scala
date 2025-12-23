package oxygen.meta

import scala.quoted.*

object ExprEvaluator {

  def evaluateExpr[A: Type](expr: Expr[A])(using Quotes): A =
    ??? // FIX-PRE-MERGE (KR) :  
  
}
