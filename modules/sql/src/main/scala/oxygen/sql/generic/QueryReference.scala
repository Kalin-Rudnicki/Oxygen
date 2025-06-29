package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] enum QueryReference {

  val param: Function.Param

  case Input(param: Function.Param)
  case Query(param: Function.Param, tableRepr: Expr[TableRepr[?]], isRoot: Boolean)

  final def show: String = this match
    case QueryReference.Input(param)           => param.name.greenFg.toString
    case QueryReference.Query(param, _, true)  => param.name.hexFg("#7EB77F").toString
    case QueryReference.Query(param, _, false) => param.name.hexFg("#FFADC6").toString

}
