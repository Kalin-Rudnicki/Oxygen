package oxygen.mcp.generic

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.*
import oxygen.mcp.domain.model.*
import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import zio.*

/**
  * Derives one [[McpTool]] per abstract method of a trait `Api` — the decoupled replacement for v1's
  * http-coupled `@mcp.tool` derivation, rebuilt on a proper codec layer that mirrors oxygen-http's
  * generic derivation ([[oxygen.http.core.generic.RouteRepr]] / `DerivedServerEndpointImpl`):
  *   - each parameter becomes an [[McpParamCodec]] (model-supplied json, or an injected principal /
  *     tool-input), which both contributes to the tool's `inputSchema` and decodes a `tools/call`;
  *   - the result is encoded through an [[McpResponseSchema.Success]] (for `A`) and, on a typed failure,
  *     an [[McpResponseSchema.Failure]] (for `E`) — a typed error is forced into a protocol [[McpError]]
  *     that surfaces at the transport (e.g. an auth error -> `401`);
  *   - `@mcpDoc` on the method / a parameter flows into the tool / property `description`.
  *
  * Each tool method must:
  *   - take a single (possibly empty) parameter list, every model-supplied param having a `JsonSchema`;
  *   - return `zio.ZIO[Scope | Any, E, A]` (e.g. `URIO[Scope, A]`), with a `JsonSchema` for `A` (and,
  *     unless `E` is `Nothing`, for `E`).
  *
  * The public entrypoint is [[oxygen.mcp.domain.DeriveMcp]] (`trait MyApi derives DeriveMcp`), which
  * calls [[derivedImpl]] to produce a `DeriveMcp[Api]` bundling the derived tools as an [[McpTools]].
  */
object McpDerive {

  def derivedImpl[Api: Type](using Quotes): Expr[DeriveMcp[Api]] = {
    val apiTpe: TypeRepr = TypeRepr.of[Api]
    val apiSym: Symbol = apiTpe.typeSymbol

    if !apiSym.flags.is(Flags.Trait) then report.errorAndAbort(s"DeriveMcp.derived: ${apiTpe.showAnsiCode} must be a trait")

    val methods: List[Symbol] =
      apiSym.declaredMethods.filter(m => m.flags.is(Flags.Deferred) && m.paramSymss.sizeIs <= 1 && m.paramSymss.forall(_.forall(!_.isTypeParam)))

    if methods.isEmpty then report.errorAndAbort(s"DeriveMcp.derived: ${apiTpe.showAnsiCode} has no abstract tool methods")

    val toolsExpr: Expr[List[McpTool[Api]]] = Expr.ofList(methods.map(m => toolExpr[Api](apiTpe, m)))
    val tagExpr: Expr[Tag[Api]] =
      Expr.summon[Tag[Api]].getOrElse(report.errorAndAbort(s"DeriveMcp: no given zio.Tag[${apiTpe.showAnsiCode}]"))

    '{
      new DeriveMcp[Api] {
        override def tools: McpTools[Api] = McpTools.of[Api]($tagExpr)(Growable.many($toolsExpr))
      }
    }
  }

  /** One derived param: its name, type, and `@mcpDoc` (if any). */
  private final case class ParamRepr(name: String, tpe: TypeRepr, doc: Option[String])

  private def toolExpr[Api: Type](apiTpe: TypeRepr, m: Symbol)(using Quotes): Expr[McpTool[Api]] = {
    val (params, resType, hasParamList): (List[ParamRepr], TypeRepr, Boolean) =
      apiTpe.memberType(m) match {
        case mt: MethodType =>
          val paramSyms: List[Symbol] = m.paramSymss.flatten.filter(s => !s.isTypeParam)
          val ps: List[ParamRepr] =
            mt.paramNames.zip(mt.paramTypes).zipWithIndex.map { case ((pn, pt), idx) =>
              ParamRepr(pn, pt.dealias, paramSyms.lift(idx).flatMap(_.annotations.optionalOfValue[mcpDoc].map(_.doc)))
            }
          (ps, mt.resType, true)
        case other =>
          (Nil, other, false)
      }

    // Robustly extract the typed error `E` / success `A` from the method's ZIO return type, mirroring
    // oxygen-http's `RouteRepr.deriveRequired` (both a `Scope`- and `Any`-env ZIO are accepted).
    val (errTpe, succTpe): (TypeRepr, TypeRepr) =
      resType.widen.dealias.asType match {
        case '[ZIO[Any, e, a]]   => (TypeRepr.of[e], TypeRepr.of[a])
        case '[ZIO[Scope, e, a]] => (TypeRepr.of[e], TypeRepr.of[a])
        case '[ZIO[r, ?, ?]]     =>
          report.errorAndAbort(s"McpDerive: tool method `${m.name}` returns a ZIO whose R type is not `Scope` or `Any`: ${TypeRepr.of[r].showAnsiCode}")
        case _ =>
          report.errorAndAbort(s"McpDerive: tool method `${m.name}` must return zio.ZIO[Scope | Any, E, A] (e.g. URIO[Scope, A]); got ${resType.showAnsiCode}")
      }

    // An `McpPrincipal` / `Option[McpPrincipal]` param is injected from the authenticated caller rather
    // than decoded from args (and excluded from the input schema); a required principal sets requiresAuth.
    val requiresAuth: Boolean = params.exists(p => isRequiredPrincipal(p.tpe))

    val toolDoc: Option[String] = m.annotations.optionalOfValue[mcpDoc].map(_.doc)

    // Every param's schema contribution (`None` for an injected param) — folded into the `inputSchema`.
    val inputParamExprs: List[Expr[Option[API.response.ToolInputSchema.Param]]] =
      params.map { p =>
        type T
        given Type[T] = p.tpe.asTypeOf
        '{ ${ codecExpr[T](m, p) }.inputParam }
      }

    val handlerExpr: Expr[Api => McpToolInput => ZIO[Scope, McpError, McpToolResult]] =
      '{ (api: Api) => (input: McpToolInput) =>
        val args: Json = input.arguments
        ${ buildInvocation[Api](m, params, hasParamList, errTpe, succTpe, 'api, 'input, 'args, Nil) }
      }

    '{
      McpTool[Api](
        tool = API.response.Tool(
          name = ${ Expr(m.name) },
          title = None,
          description = ${ Expr(toolDoc) },
          inputSchema = API.response.ToolInputSchema.fromParams(${ Expr.ofList(inputParamExprs) }.flatten),
          outputSchema = None,
          annotations = None,
          icons = None,
        ),
        requiresAuth = ${ Expr(requiresAuth) },
        handle = $handlerExpr,
      )
    }
  }

  private def isRequiredPrincipal(using Quotes)(pt: TypeRepr): Boolean = pt =:= TypeRepr.of[McpPrincipal]
  private def isOptionalPrincipal(using Quotes)(pt: TypeRepr): Boolean = pt =:= TypeRepr.of[Option[McpPrincipal]]

  /**
    * The [[McpParamCodec]] for one param: an injected principal / tool-input, or — for a model-supplied
    * param — the summoned name-agnostic [[McpFieldParamCodec]] `.named` with the param name (+ `@mcpDoc`).
    */
  private def codecExpr[T: Type](m: Symbol, p: ParamRepr)(using Quotes): Expr[McpParamCodec[T]] =
    if isRequiredPrincipal(p.tpe) then '{ McpParamCodec.principal }.asExprOf[McpParamCodec[T]]
    else if isOptionalPrincipal(p.tpe) then '{ McpParamCodec.optionalPrincipal }.asExprOf[McpParamCodec[T]]
    else if p.tpe =:= TypeRepr.of[McpToolInput] then '{ McpParamCodec.toolInput }.asExprOf[McpParamCodec[T]]
    else {
      val field: Expr[McpFieldParamCodec[T]] = summonFieldCodec[T](s"param `${p.name}` of tool `${m.name}`")
      '{ $field.named(${ Expr(p.name) }, ${ Expr(p.doc) }) }
    }

  private def buildInvocation[Api: Type](
      m: Symbol,
      params: List[ParamRepr],
      hasParamList: Boolean,
      errTpe: TypeRepr,
      succTpe: TypeRepr,
      apiExpr: Expr[Api],
      inputExpr: Expr[McpToolInput],
      argsExpr: Expr[Json],
      decodedTerms: List[Term],
  )(using Quotes): Expr[ZIO[Scope, McpError, McpToolResult]] =
    params match {
      case p :: rest =>
        type T
        given Type[T] = p.tpe.asTypeOf
        val codec: Expr[McpParamCodec[T]] = codecExpr[T](m, p)
        '{
          $codec.decode($argsExpr, $inputExpr) match {
            case Left(err)    => ZIO.fail(err)
            case Right(value) => ${ buildInvocation[Api](m, rest, hasParamList, errTpe, succTpe, apiExpr, inputExpr, argsExpr, 'value.toTerm :: decodedTerms) }
          }
        }
      case Nil =>
        type E
        type A
        given Type[E] = errTpe.asTypeOf
        given Type[A] = succTpe.asTypeOf
        val select: Term = apiExpr.toTerm.select(m)
        val callTerm: Term = if hasParamList then select.appliedToArgs(decodedTerms.reverse) else select
        val call: Expr[ZIO[Scope, E, A]] = callTerm.asExprOf[ZIO[Scope, E, A]]
        val success: Expr[McpResponseSchema.Success[A]] = summonResponse[McpResponseSchema.Success[A]](s"result of tool `${m.name}`")
        val failure: Expr[McpResponseSchema.Failure[E]] = summonResponse[McpResponseSchema.Failure[E]](s"error of tool `${m.name}`")
        '{ McpResponseSchema.run[E, A]($call, $success, $failure) }
    }

  private def summonFieldCodec[T: Type](what: String)(using Quotes): Expr[McpFieldParamCodec[T]] =
    Expr.summon[McpFieldParamCodec[T]].getOrElse(report.errorAndAbort(s"McpDerive: no given oxygen.mcp.domain.McpFieldParamCodec[${Type.show[T]}] for $what"))

  private def summonResponse[T: Type](what: String)(using Quotes): Expr[T] =
    Expr.summon[T].getOrElse(report.errorAndAbort(s"McpDerive: no given ${TypeRepr.of[T].showAnsiCode} for $what"))

}
