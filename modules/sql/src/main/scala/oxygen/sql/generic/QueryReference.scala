package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] sealed trait QueryReference {

  val param: Function.Param

  final def show: String = this match
    case QueryReference.InputParam(param)      => param.name.greenFg.toString
    case QueryReference.ConstInput(param, _)   => s"const(${param.name.greenFg})" // TODO (KR) : show differently?
    case QueryReference.Query(param, _, true)  => param.name.hexFg("#7EB77F").toString
    case QueryReference.Query(param, _, false) => param.name.hexFg("#FFADC6").toString

}
private[generic] object QueryReference {

  sealed trait InputLike extends QueryReference {

    val param: Function.Param

    trait InputTransformer extends TermTransformer {
      override final def outTpe: TypeRepr = param.outTpe
    }

    def inputTransformer(inputTpe: TypeRepr, idx: Int)(using Quotes): InputTransformer

  }

  final case class InputParam(param: Function.Param) extends InputLike {

    override def inputTransformer(inputTpe: TypeRepr, idx: Int)(using Quotes): InputTransformer =
      new InputTransformer {
        override def inTpe: TypeRepr = inputTpe
        override protected def convertTermInternal(term: Term)(using Quotes): Term = term.select(s"_${idx + 1}")
      }

  }

  final case class ConstInput(param: Function.Param, term: Term) extends InputLike { self =>

    override def inputTransformer(inputTpe: TypeRepr, idx: Int)(using Quotes): InputTransformer =
      new InputTransformer {
        override def inTpe: TypeRepr = TypeRepr.of[Any]
        override protected def convertTermInternal(term: Term)(using Quotes): Term = self.term
      }

  }

  final case class Query(param: Function.Param, tableRepr: Expr[TableRepr[?]], isRoot: Boolean) extends QueryReference

}
