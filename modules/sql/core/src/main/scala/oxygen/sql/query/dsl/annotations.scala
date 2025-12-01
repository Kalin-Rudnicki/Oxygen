package oxygen.sql.query.dsl

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.query.*
import scala.annotation.{experimental, MacroAnnotation}
import scala.quoted.*

@experimental
final class compile(debug: Boolean = false) extends MacroAnnotation {

  private def transformValDef(valDef: ValDef)(using Quotes): ValDef = {
    val newRHS: Term = CompileMacros(Expr(valDef.name), valDef.requiredRHS.asExprOf[QueryLike], Expr(debug)).toTerm
    val newValDef: ValDef = ValDef.companion.apply(valDef.symbol, newRHS.some)
    newValDef
  }

  private def transform(definition: Definition)(using Quotes): Definition =
    definition match {
      case valDef: ValDef => transformValDef(valDef)
      // TODO (KR) : support @compile on an object, is currently throwing weird errors
      /*
      case classDef: ClassDef if classDef.symbol.flags.is(Flags.Module) =>
        val newBody: List[(Boolean, Statement)] =
          classDef.body.map {
            case valDef: ValDef if valDef.tpt.tpe.widen <:< TypeRepr.of[QueryLike] => (true, transformValDef(valDef))
            case s                                                                 => (false, s)
          }
        if (newBody.exists(_._1))
          ClassDef.copy(classDef)(
            classDef.name,
            classDef.constructor,
            classDef.parents,
            classDef.self,
            newBody.map(_._2),
          )
        else {
          report.warning("no queries were detected or compiled", classDef.symbol.pos)
          classDef
        }
       */
      case _ =>
        report.errorAndAbort(
          s""" 
             |`@compile` can not compile a `${definition.getClass.getName}`
             | 
             |only supports:
             | 
             |@compile
             |val a: ... = ...
             | """.stripMargin,
        )
    }

  override def transform(using quotes: Quotes)(definition: quotes.reflect.Definition, companion: Option[quotes.reflect.Definition]): List[quotes.reflect.Definition] = {
    if companion.nonEmpty then report.errorAndAbort("error, annotated term has a companion, not supported")
    transform(Definition.wrap(using quotes)(definition)).unwrapWithin :: Nil
  }

}
