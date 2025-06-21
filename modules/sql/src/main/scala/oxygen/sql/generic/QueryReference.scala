package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] enum QueryReference {

  val param: Function.Param

  case Input(param: Function.Param, idx: Option[Int])
  case Query(param: Function.Param, schema: Expr[TableRepr[?, ?]], isRoot: Boolean)

  final def show: String = this match
    case QueryReference.Input(param, idx)           => param.name.greenFg.toString
    case QueryReference.Query(param, schema, true)  => param.name.hexFg("#7EB77F").toString
    case QueryReference.Query(param, schema, false) => param.name.hexFg("#FFADC6").toString

}
