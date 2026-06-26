package oxygen.http.server.generic

import oxygen.http.core.{LineStream, ServerSentEvents}
import oxygen.http.core.generic.*
import oxygen.http.schema.McpEndpointSchema
import oxygen.http.server.*
import oxygen.http.server.mcp.*
import oxygen.json.Json
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.schema.{JsonSchema, PlainTextSchema}
import scala.quoted.*
import zio.*

sealed trait EndpointRepr[Api] {

  val route: RouteRepr[Api]
  import route.given

  type In = route.In

  protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]]

  final def withImpl[O: Type](f: EndpointRepr.WithImpl[Api] => Expr[O])(using Quotes): Expr[O] =
    ValDef.companion.letExpr[DerivedServerEndpointImpl[Api, In], O](
      route.defDef.name + "__Impl",
      makeImpl,
      ValDef.ValType.LazyVal,
    ) { expr => f(EndpointRepr.WithImpl(this)(expr)) }

  protected final def serverErrorHandlerExpr(using Quotes): Expr[ServerErrorHandler[route.ErrorOut]] =
    Implicits.searchOption[ServerErrorHandler[route.ErrorOut]].getOrElse { route.fail(s"No given instance found for: ${TypeRepr.of[ServerErrorHandler[route.ErrorOut]].showAnsiCode}") }

}
object EndpointRepr {

  final class WithImpl[Api](val endpointRepr: EndpointRepr[Api])(val impl: Expr[DerivedServerEndpointImpl[Api, endpointRepr.In]]) {
    import endpointRepr.route.given

    def toEndpoint(using Quotes): Expr[Endpoint[Api]] =
      '{ $impl.toEndpoint }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromZIO
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromZIO[Api](val route: RouteRepr.ReturningZIO[Api]) extends EndpointRepr[Api] {
    import route.given

    /////// MCP (@mcp.tool / @mcp.auth) ///////////////////////////////////////////////////////////////

    /** Function-param names annotated `@mcp.auth` (the params fed by the MCP bearer token). */
    lazy val mcpAuthParamNames: List[String] =
      route.valDefs.filter { vd =>
        vd.symbol.annotations.optionalOf[oxygen.http.core.mcp] match
          case None       => false
          case Some(expr) =>
            expr match
              case '{ new oxygen.http.core.`mcp`.`auth`() }   => true
              case '{ new oxygen.http.core.`mcp`.`tool`() }   => route.failParam(vd, "@mcp.tool cannot be applied to a param — put it on the route")
              case '{ new oxygen.http.core.`mcp`.`tool`($_) } => route.failParam(vd, "@mcp.tool cannot be applied to a param — put it on the route")
              case _                                          => route.failParam(vd, s"invalid @mcp.___ annotation on param\n\n${expr.showAnsiCode}")
      }.map(_.name)

    locally {
      if route.mcpToolName.isEmpty && mcpAuthParamNames.nonEmpty then route.fail("@mcp.auth requires the route to be annotated with @mcp.tool")
      if mcpAuthParamNames.sizeIs > 1 then route.fail("at most one @mcp.auth param is allowed per endpoint")
    }

    /** The derived [[McpEndpointSchema]] (or `None` if not `@mcp.tool`), as an expr for the endpoint impl. */
    def mcpPartsExpr(using Quotes): Expr[Option[DerivedServerEndpointImpl.McpParts[route.In, route.ErrorOut, route.SuccessOut]]] =
      route.mcpToolName match {
        case Some(mcpToolName) =>
          '{
            val requestCodec: McpRequestCodec[route.In] = $mcpRequestCodecExpr

            DerivedServerEndpointImpl.McpParts[route.In, route.ErrorOut, route.SuccessOut](
              schema = McpEndpointSchema(
                toolName = ${ Expr(mcpToolName) },
                description = ${ Expr(route.defDefDoc) },
                inputSchema = McpRequestCodec.inputSchema(requestCodec),
                authParamName = ${
                  mcpAuthParamNames.headOption match
                    case Some(paramName) => '{ ${ Expr(paramName) }.some }
                    case None            => '{ None }
                },
              ),
              requestCodec = requestCodec,
              errorResponseCodec = ${
                Implicits.searchOption[McpResponseCodec[route.ErrorOut]].getOrElse {
                  route.fail(s"@mcp.tool: no given oxygen.schema.JsonSchema for the error type ${TypeRepr.of[route.ErrorOut].showAnsiCode} (needed to encode the tool result)")
                }
              },
              successResponseCodec = ${
                Implicits.searchOption[McpResponseCodec[route.SuccessOut]].getOrElse {
                  route.fail(s"@mcp.tool: no given oxygen.schema.JsonSchema for the success type ${TypeRepr.of[route.SuccessOut].showAnsiCode} (needed to encode the tool result)")
                }
              },
            ).some
          }
        case None =>
          '{ None }
      }

    /**
      * The MCP tool codec: decodes a `tools/call` JSON `arguments` object directly into [[In]] (no HTTP
      * round-trip). Non-auth params decode from `args[name]` via their `JsonSchema`; the `@mcp.auth`
      * param decodes from the validated bearer via its `PlainTextSchema`.
      */
    def mcpRequestCodecExpr(using Quotes): Expr[McpRequestCodec[In]] = {
      val inputParamExprs: List[Expr[McpRequestCodec.InputParam]] =
        route.functionParamsInParamOrder.toList.flatMap { p =>
          if mcpAuthParamNames.contains(p.valDef.name) then None
          else {
            type T
            given Type[T] = p.tpe.asInstanceOf[Type[T]]
            val js: Expr[JsonSchema[T]] = summonMcpJsonSchema[T](p)
            val required: Boolean = !(p.typeRepr <:< TypeRepr.of[Option[Any]])
            Some('{ McpRequestCodec.InputParam(${ Expr(p.valDef.name) }, $js, ${ Expr(required) }, ${ Expr(p.doc) }) })
          }
        }

      '{
        new McpRequestCodec[In] {
          override val inputParams: ArraySeq[McpRequestCodec.InputParam] = ArraySeq.from(${ Expr.ofList(inputParamExprs) })
          override def decode(in: McpInput): Either[String, In] =
            ${ mcpDecodeRec('{ in.arguments }, '{ in.auth }, route.functionParamsInParseOrder.toList, Nil) }
        }
      }
    }

    private def mcpDecodeRec(
        argsExpr: Expr[Json],
        bearerExpr: Expr[Option[String]],
        queue: List[ParamRepr.FunctionArg[?]],
        rTerms: List[Term],
    )(using Quotes): Expr[Either[String, In]] =
      queue match {
        case _head :: tail =>
          type T
          val head: ParamRepr.FunctionArg[T] = _head.asInstanceOf[ParamRepr.FunctionArg[T]]
          given Type[T] = head.tpe

          val decodeOne: Expr[Either[String, T]] =
            if mcpAuthParamNames.contains(head.valDef.name) then
              '{ McpRequestCodec.decodeAuthArg[T](${ summonMcpPlainTextSchema[T](head) }, $bearerExpr) }
            else
              '{ McpRequestCodec.decodeJsonArg[T](${ summonMcpJsonSchema[T](head) }.jsonDecoder, $argsExpr, ${ Expr(head.valDef.name) }) }

          '{ $decodeOne.flatMap { value => ${ mcpDecodeRec(argsExpr, bearerExpr, tail, 'value.toTerm :: rTerms) } } }
        case Nil =>
          '{ (Right(${ route.parseOrderTermsToInExpr(rTerms.reverse) }): Either[String, In]) }
      }

    private def summonMcpJsonSchema[T: Type](fa: ParamRepr.FunctionArg[?])(using Quotes): Expr[JsonSchema[T]] =
      Implicits.searchOption[JsonSchema[T]]
        .getOrElse(route.failParam(fa.valDef, s"@mcp.tool: no given oxygen.schema.JsonSchema[${TypeRepr.of[T].showAnsiCode}] for param `${fa.valDef.name}`"))

    private def summonMcpPlainTextSchema[T: Type](fa: ParamRepr.FunctionArg[?])(using Quotes): Expr[PlainTextSchema[T]] =
      Implicits.searchOption[PlainTextSchema[T]]
        .getOrElse(route.failParam(fa.valDef, s"@mcp.auth: no given oxygen.schema.PlainTextSchema[${TypeRepr.of[T].showAnsiCode}] for param `${fa.valDef.name}`"))

    override protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]] =
      '{
        DerivedServerEndpointImpl.FromZIO[Api, route.In, route.ErrorOut, route.SuccessOut](
          apiName = ${ Expr(route.derivedApiName) },
          endpointName = ${ Expr(route.derivedEndpointName) },
          doc = ${ Expr(route.defDefDoc) },
          requestCodec = ${ route.requestCodec },
          errorResponseCodec = ${ route.errorResponseCodec },
          successResponseCodec = ${ route.successResponseCodec },
          serverErrorHandler = $serverErrorHandlerExpr,
          mcp = $mcpPartsExpr,
          impl = { (api: Api, in: route.In) =>
            ${
              val apiTerm: Term = 'api.toTerm
              val inExpr: Expr[route.In] = 'in
              val paramTerms: List[Term] = route.inExprToParamOrderTerms(inExpr)
              val appliedTerm: Term = apiTerm.select(route.defDef.symbol).appliedToArgs(paramTerms)
              appliedTerm.asExprOf[ZIO[Scope, route.ErrorOut, route.SuccessOut]]
            }
          },
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromSSE
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromSSE[Api](val route: RouteRepr.ReturningSSE[Api]) extends EndpointRepr[Api] {
    import route.given

    override protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]] =
      '{
        DerivedServerEndpointImpl.FromSSE[Api, route.In, route.ErrorOut, route.SuccessOut](
          apiName = ${ Expr(route.derivedApiName) },
          endpointName = ${ Expr(route.derivedEndpointName) },
          doc = ${ Expr(route.defDefDoc) },
          requestCodec = ${ route.requestCodec },
          errorResponseCodec = ${ route.errorResponseCodec },
          schema = ${ route.successSchema },
          serverErrorHandler = $serverErrorHandlerExpr,
          impl = { (api: Api, in: route.In) =>
            ${
              val apiTerm: Term = 'api.toTerm
              val inExpr: Expr[route.In] = 'in
              val paramTerms: List[Term] = route.inExprToParamOrderTerms(inExpr)
              val appliedTerm: Term = apiTerm.select(route.defDef.symbol).appliedToArgs(paramTerms)
              appliedTerm.asExprOf[ServerSentEvents[route.ErrorOut, route.SuccessOut]]
            }
          },
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromLines
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class FromLines[Api](val route: RouteRepr.ReturningLines[Api]) extends EndpointRepr[Api] {
    import route.given

    override protected def makeImpl(using Quotes): Expr[DerivedServerEndpointImpl[Api, In]] =
      '{
        DerivedServerEndpointImpl.FromLines[Api, route.In, route.ErrorOut, route.SuccessOut](
          apiName = ${ Expr(route.derivedApiName) },
          endpointName = ${ Expr(route.derivedEndpointName) },
          doc = ${ Expr(route.defDefDoc) },
          requestCodec = ${ route.requestCodec },
          errorResponseCodec = ${ route.errorResponseCodec },
          schema = ${ route.successSchema },
          serverErrorHandler = $serverErrorHandlerExpr,
          impl = { (api: Api, in: route.In) =>
            ${
              val apiTerm: Term = 'api.toTerm
              val inExpr: Expr[route.In] = 'in
              val paramTerms: List[Term] = route.inExprToParamOrderTerms(inExpr)
              val appliedTerm: Term = apiTerm.select(route.defDef.symbol).appliedToArgs(paramTerms)
              appliedTerm.asExprOf[LineStream[route.ErrorOut, route.SuccessOut]]
            }
          },
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Entry/Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply[Api](route: RouteRepr[Api]): EndpointRepr[Api] =
    route match
      case route: RouteRepr.ReturningZIO[Api] => EndpointRepr.FromZIO[Api](route)
      case route: RouteRepr.ReturningSSE[Api] =>
        if route.mcpToolName.nonEmpty then route.fail("@mcp.tool cannot be applied to an SSE (streaming) endpoint — `tools/call` returns a single result.")
        EndpointRepr.FromSSE[Api](route)
      case route: RouteRepr.ReturningLines[Api] =>
        if route.mcpToolName.nonEmpty then route.fail("@mcp.tool cannot be applied to a LineStream (streaming) endpoint — `tools/call` returns a single result.")
        EndpointRepr.FromLines[Api](route)

}
