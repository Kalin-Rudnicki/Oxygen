package oxygen.meta

import scala.quoted.*

given toExprT_to_toExpr: [A: {Type as tpe, ToExprT as tet}] => ToExpr[A] = ToExprT.applied(tpe, tet)
given fromExprT_to_fromExpr: [A: {Type as tpe, FromExprT as fet}] => FromExpr[A] = FromExprT.applied(tpe, fet)
