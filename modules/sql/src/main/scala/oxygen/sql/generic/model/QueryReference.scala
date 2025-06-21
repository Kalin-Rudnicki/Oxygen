package oxygen.sql.generic.model

import oxygen.predef.color.given
import oxygen.quoted.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] sealed trait QueryReference {

  val param: Function.Param

  final def show: String = this match
    case QueryReference.InputParam(param)          => param.name.greenFg.toString
    case QueryReference.ConstInput(param, term, _) => s"const(${param.name.greenFg} = ${term.showAnsiCode})"
    case QueryReference.Query(param, _, true)      => param.name.hexFg("#7EB77F").toString
    case QueryReference.Query(param, _, false)     => param.name.hexFg("#FFADC6").toString

}
private[generic] object QueryReference {

  sealed trait InputLike extends QueryReference, TermTransformer {
    val param: Function.Param
  }

  final case class InputParam(param: Function.Param) extends InputLike, TermTransformer.DeferToParam

  final case class ConstInput(param: Function.Param, term: Term, anyTpe: TypeRepr) extends InputLike, TermTransformer.Die

  final case class Query(param: Function.Param, tableRepr: Expr[TableRepr[?]], isRoot: Boolean) extends QueryReference

}
