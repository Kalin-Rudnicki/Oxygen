package oxygen.sql.generic.model.full

import oxygen.meta.{*, given}
import oxygen.meta.k0.ProductGeneric
import oxygen.predef.core.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.query.*
import oxygen.sql.query.QueryContext.QueryType
import oxygen.sql.schema.*
import scala.quoted.*

private[full] object Util {

  given ToExprT[QueryContext.QueryType] = ToExprT.derived[QueryContext.QueryType]

  def showInputs(inputs: List[InputPart]): String =
    if inputs.nonEmpty then inputs.map(_.show).mkString("inputs:", "", "\n")
    else "<no inputs>\n"

  def showReturning(ret: ReturningPart)(using Quotes): String =
    ret.showOpt.map { ret => s"\n    RETURNING $ret" }.mkString

  def makeQuery(
      queryName: Expr[String],
      queryType: QueryContext.QueryType,
      mainTable: Option[TypeclassExpr.TableRepr],
      @scala.annotation.unused debug: Boolean,
  )(
      sql: GeneratedSql.Built,
      encoder: GeneratedInputEncoder.Built,
      decoder: GeneratedResultDecoder.Built,
  )(using Quotes): Expr[QueryLike] = {
    type I
    type O
    given Type[I] = encoder.tpe.asTypeOf
    given Type[O] = decoder.tpe.asTypeOf

    val ctx: Expr[QueryContext] =
      '{
        QueryContext(
          queryName = $queryName,
          sql = ${ sql.sql },
          queryType = ${ Expr(queryType) },
          mainTable = ${
            mainTable match {
              case Some(mainTable) => '{ ${ mainTable.expr }.some }
              case None            => '{ None }
            }
          },
        )
      }

    (encoder.hasNonConstParams, encoder.hasParams, decoder.nonEmpty) match {
      case (true, _, true) => // QueryIO - non const inputs
        '{
          new QueryIO.Simple[I, O](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[I]] },
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (false, true, true) => // QueryO - with const inputs
        '{
          new QueryO.Simple[O](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[Any]] }.some,
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (false, false, true) => // QueryO - no inputs
        '{
          new QueryO.Simple[O](
            ctx = $ctx,
            encoder = None,
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (true, _, false) => // QueryI - non const inputs
        '{
          new QueryI[I](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[I]] },
          )
        }
      case (false, true, false) => // Query - with const inputs
        '{
          new Query(
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[Any]] }.some,
          )
        }
      case (false, false, false) => // Query - no inputs
        '{
          new Query(
            ctx = $ctx,
            encoder = None,
          )
        }
    }
  }

}
