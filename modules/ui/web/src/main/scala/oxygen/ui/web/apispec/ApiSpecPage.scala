package oxygen.ui.web.apispec

import oxygen.core.syntax.string.*
import oxygen.http.client.RawClient
import oxygen.http.schema.compiled.*
import oxygen.json.JsonCodec
import oxygen.schema.compiled.{CompiledSchemaRef, FullCompiledJsonSchema, FullCompiledPlainSchema, FullCompiledSchema}
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*
import zio.http.{Request, URL}

/**
  * A reusable, read-only page that fetches the compiled oxygen-http API spec (served by
  * `ApiSpecEndpointMiddleware` at `/oxygen/api-spec`) and renders the endpoints + resolved schema
  * graph using the oxygen.ui.web component library, in a Swagger-style layout.
  *
  * Fetch strategy (decision "A"): a raw GET via `RawClient` + decode with `JsonCodec`, so no
  * `JsonSchema` instances are needed across the compiled model.
  */
object ApiSpecPage extends RoutablePage.NoParams[RawClient] {

  /** Server path the spec is served from (the default `ApiSpecEndpointMiddleware` mount). */
  val specPath: String = "/oxygen/api-spec"

  /** UI route (under the app's page prefix). */
  override val path: Seq[String] = Seq("api-spec")

  final case class PageState(spec: FullCompiledApiSpec)

  override def title(state: PageState): String = "Oxygen API Spec"

  override def initialLoad(params: PageParams): ZIO[RawClient & Scope, UIError, PageState] = {
    val fetch: ZIO[RawClient & Scope, Throwable, PageState] =
      for {
        rawClient <- ZIO.service[RawClient]
        url <- ZIO.fromEither(URL.decode(specPath))
        response <- rawClient.client.request(Request.get(url))
        body <- response.body.asString
        raw <- ZIO.fromEither(JsonCodec[RawCompiledApiSpec].decoder.decodeJsonString(body)).mapError(e => new RuntimeException(e.toString))
      } yield PageState(raw.toFullCompiledApiSpec)

    fetch.mapError(UIError.ClientSide.InternalDefect.somethingWentWrong(_))
  }

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[RawClient & Scope, UIError, Unit] =
    ZIO.unit

  // lazy: NavBar.make reads css-vars, which are only populated after stylesheets are registered
  private lazy val navBar: NavBar.Const =
    NavBar.make()(_.apply("Oxygen API Spec"))()

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[RawClient, PageState] =
    Widget.state[PageState].get { st =>
      val full = st.spec
      val endpointCount: Int = full.apis.map(_.endpoints.size).sum
      div(
        display.flex,
        flexDirection.column,
        height := 100.vh,
        width := 100.vw,
        overflow.hidden,
        navBar.widget,
        div( // scroll area
          flexGrow := 1,
          minHeight := 0.px,
          overflowX.hidden,
          OxygenStyleSheet.Scrollable, // overflow-y: auto + themed scrollbar styling
          backgroundColor := S.color.bg.base,
          div( // centered content column
            maxWidth := 1000.px,
            margin := "0 auto",
            padding := css(S.spacing._8, S.spacing._6),
            pageHeading("API Reference"),
            p(
              s"$endpointCount endpoints · ${full.schemas.allSchemas.size} schemas",
              color := S.color.fg.subtle,
              margin := css(S.spacing._2, "0", S.spacing._6, "0"),
            ),
            Widget.foreach(full.apis)(renderApi(full, _)),
            sectionHeading("Schemas"),
            Widget.foreach(orderedSchemas(full))(renderSchema),
          ),
        ),
      )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Style helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def mono = fontFamily := "monospace"

  private def card: Node =
    div(
      backgroundColor := S.color.bg.layerOne,
      borderRadius := S.borderRadius._5,
      padding := css(S.spacing._5, S.spacing._6),
      margin := css(S.spacing._4, "0"),
      boxShadow := "0 1px 2px rgba(0,0,0,0.12)",
    )

  private def pageHeading(text: String): Node =
    h1(text, color := S.color.fg.default, margin := css(S.spacing._2, "0"))

  private def sectionHeading(text: String): Node =
    h2(
      text,
      color := S.color.fg.default,
      margin := css(S.spacing._10, "0", S.spacing._4, "0"),
      padding := css("0", "0", S.spacing._2, "0"),
      borderBottom := s"2px solid ${S.color.bg.layerThree}",
    )

  private def methodColor(method: String): String =
    method.toUpperCase match
      case "GET"    => "#61affe"
      case "POST"   => "#49cc90"
      case "PUT"    => "#fca130"
      case "DELETE" => "#f93e3e"
      case "PATCH"  => "#50e3c2"
      case "HEAD"   => "#9012fe"
      case _        => "#7d8492"

  private def methodBadge(method: String): Node =
    span(
      method.toUpperCase,
      display.inlineBlock,
      backgroundColor := methodColor(method),
      color := "#ffffff",
      fontWeight.bold,
      fontSize := 12.px,
      letterSpacing := "0.5px",
      padding := "5px 12px",
      borderRadius := 6.px,
      minWidth := 62.px,
      textAlign.center,
    )

  private def tagPill(text: String): Node =
    span(
      text,
      display.inlineBlock,
      border := s"1px solid ${S.color.fg.minimal}",
      color := S.color.fg.moderate,
      fontSize := 11.px,
      textTransform.uppercase,
      letterSpacing := "0.5px",
      padding := "2px 8px",
      borderRadius := 10.px,
    )

  /** "requestConnection" -> "Request Connection" */
  private def humanize(name: String): String =
    name.camelToSnake.split('_').filter(_.nonEmpty).map(_.capitalize).mkString(" ")

  // semantic pill colors (filled, white text — meant to stand out)
  private def requiredColor: String = "#e0533f" // red-orange
  private def optionalColor: String = "#6b7280" // gray
  private def repeatedColor: String = "#2f81f7" // blue
  private def nullableColor: String = "#a855f7" // purple

  private def semanticPill(text: String, bg: String): Node =
    span(
      text,
      display.inlineBlock,
      backgroundColor := bg,
      color := "#ffffff",
      fontWeight._600,
      fontSize := 10.px,
      textTransform.uppercase,
      letterSpacing := "0.5px",
      padding := "2px 7px",
      borderRadius := 10.px,
    )

  /** Presence/cardinality pills for a request param. */
  private def paramKindPills(kind: CompiledParamType): Seq[Node] =
    kind match
      case CompiledParamType.Required     => Seq(semanticPill("required", requiredColor))
      case CompiledParamType.Optional     => Seq(semanticPill("optional", optionalColor))
      case CompiledParamType.ManyRequired => Seq(semanticPill("required", requiredColor), semanticPill("repeated", repeatedColor))
      case CompiledParamType.ManyOptional => Seq(semanticPill("optional", optionalColor), semanticPill("repeated", repeatedColor))

  /**
    * Presence + nullability pills for an object field. These are orthogonal:
    *   - presence (`onMissing`): required (must be present) vs optional (may be omitted)
    *   - nullability (`nullable`): whether an explicit `null` is an accepted value
    */
  private def fieldPills(f: FullCompiledJsonSchema.ProductField): Seq[Node] = {
    val presence: Node = f.onMissing match
      case None    => semanticPill("required", requiredColor)
      case Some(_) => semanticPill("optional", optionalColor)
    val nullable: Seq[Node] = if f.nullable then Seq(semanticPill("nullable", nullableColor)) else Nil
    presence +: nullable
  }

  /** Flatten the (de-emphasized) expected statuses into display tokens: explicit codes or labels. */
  private def statusTokens(s: CompiledExpectedStatuses): Seq[Either[Int, String]] =
    s match
      case CompiledExpectedStatuses.None              => Seq(Right("none"))
      case CompiledExpectedStatuses.All               => Seq(Right("any"))
      case CompiledExpectedStatuses.Exact(code)       => Seq(Left(code))
      case CompiledExpectedStatuses.OneOf(codes)      => codes.sorted.map(Left(_))
      case CompiledExpectedStatuses.Range(name, _, _) => Seq(Right(name))
      case CompiledExpectedStatuses.Or(a, b)          => statusTokens(a) ++ statusTokens(b)

  private def statusColor(code: Int): String =
    if code < 300 then "#49cc90" // 2xx green
    else if code < 400 then "#61affe" // 3xx blue
    else if code < 500 then "#fca130" // 4xx orange
    else "#f93e3e" // 5xx red

  private def statusCodePill(code: Int): Node = {
    val c = statusColor(code)
    span(
      code.toString,
      display.inlineBlock,
      border := s"1px solid $c",
      color := c,
      fontWeight._600,
      fontSize := 13.px,
      padding := "1px 9px",
      borderRadius := 10.px,
    )
  }

  private def statusNodes(s: CompiledExpectedStatuses): Seq[Node] =
    statusTokens(s).map {
      case Left(code)  => statusCodePill(code)
      case Right(text) => span(text, color := S.color.fg.subtle, fontSize := 12.px, fontStyle.italic)
    }

  private def subLabelSpan(text: String): Node =
    span(text, fontWeight.bold, fontSize := 13.px, textTransform.uppercase, letterSpacing := "0.5px", color := S.color.fg.moderate)

  private def anchorId(ref: CompiledSchemaRef): String =
    "schema-" + ref.primaryReference.fullTypeName.replaceAll("[^A-Za-z0-9]+", "-")

  /** A type reference rendered as a monospace link that jumps to its schema definition. */
  /** A monospace link whose text is `text` and which jumps to `ref`'s schema definition. */
  private def typeLink(text: String, ref: CompiledSchemaRef): Node =
    a(
      text,
      href := s"#${anchorId(ref)}",
      mono,
      fontSize := 13.px,
      color := S.color.fg.textLink,
      textDecoration := "none",
    )

  private def typeChip(ref: CompiledSchemaRef): Node = typeLink(ref.showBase, ref)

  private def subLabel(text: String): Node =
    div(
      text,
      fontWeight.bold,
      fontSize := 13.px,
      textTransform.uppercase,
      letterSpacing := "0.5px",
      color := S.color.fg.moderate,
      margin := css(S.spacing._3, "0", S.spacing._1, "0"),
    )

  private def row(content: Widget*): Node =
    div(
      display.flex,
      alignItems.center,
      gap := S.spacing._3,
      margin := css(S.spacing._1, "0"),
    )(content*)

  /** One row of an attribute table: name, type, and presence/nullability pills (+ optional doc). */
  private final case class Attr(name: String, typeWidget: Widget, pills: Seq[Node], doc: Option[String])

  /** Renders attributes (params / object fields) as an aligned 3-column table: name | type | pills. */
  private def attrTable(rows: Seq[Attr]): Node = {
    def cell(rightPad: Boolean)(content: Widget*): Node =
      td(verticalAlign.top, padding := css(S.spacing._1, if rightPad then S.spacing._6 else "0", S.spacing._1, "0"))(content*)
    table(borderCollapse.collapse, margin := css(S.spacing._1, "0", S.spacing._2, "0"))(
      Widget.foreach(rows.toList) { r =>
        tr(
          cell(true)(span(r.name, mono, fontWeight.bold, fontSize := 13.px, color := S.color.fg.default)),
          cell(true)(r.typeWidget),
          cell(false)(
            div(display.flex, alignItems.center, flexWrap.wrap, gap := S.spacing._2)(
              (r.pills ++ r.doc.toSeq.map(d => span(d, color := S.color.fg.subtle, fontSize := 12.px)))*,
            ),
          ),
        )
      },
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Endpoint rendering
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def renderApi(full: FullCompiledApiSpec, api: RawCompiledApi): Node =
    div(
      sectionHeading(api.name.getOrElse("General")),
      Widget.foreach(api.endpoints)(renderEndpoint(full, _)),
    )

  private def renderEndpoint(full: FullCompiledApiSpec, ep: RawCompiledEndpoint): Node = {
    val method: String = ep.request.method.fold("ANY")(_.name)
    val pathStr: String = ep.request.paths.toList.map(renderPath).mkString("  |  ")
    card(
      row(
        methodBadge(method),
        span(humanize(ep.name), fontWeight.bold, fontSize := 16.px, color := S.color.fg.default),
        span(pathStr, mono, fontSize := 14.px, color := S.color.fg.moderate),
      ),
      ep.doc.fold(fragment)(d => p(d, color := S.color.fg.moderate, margin := css(S.spacing._3, "0"))),
      paramBlock("Path parameters", ep.request.paths.toList.flatMap(pathParams)),
      paramBlock("Query parameters", ep.request.queryParams.toList),
      paramBlock("Headers", ep.request.headers.toList),
      requestBodyBlock(ep.request.body),
      responseBlock(full, "Success", ep.successResponse),
      responseBlock(full, "Error", ep.errorResponse),
    )
  }

  private def renderPath(paths: RawCompiledPaths): String = {
    val segs: String =
      paths.segments
        .map {
          case RawCompiledPathSegment.Const(p)       => p
          case RawCompiledPathSegment.Param(n, _, _) => s"{$n}"
        }
        .mkString("/", "/", "")
    paths.rest.fold(segs)(r => s"$segs/{${r.name}...}")
  }

  private def pathParams(paths: RawCompiledPaths): List[RawCompiledParam] =
    paths.segments.toList.collect { case RawCompiledPathSegment.Param(n, doc, ref) => RawCompiledParam(n, doc, CompiledParamType.Required, ref) } :::
      paths.rest.toList.map(r => RawCompiledParam(r.name, r.doc, if r.required then CompiledParamType.ManyRequired else CompiledParamType.ManyOptional, r.schema))

  private def paramBlock(label: String, params: List[RawCompiledParam]): Widget =
    if params.isEmpty then fragment
    else
      div(
        subLabel(label),
        attrTable(params.map(p => Attr(p.name, typeChip(p.schema), paramKindPills(p.kind), p.doc))),
      )

  private def requestBodyBlock(body: RawCompiledRequestBody): Widget =
    body match
      case RawCompiledRequestBody.Empty               => fragment
      case RawCompiledRequestBody.Single(n, doc, ref) =>
        div(
          subLabel("Request body"),
          attrTable(Seq(Attr(n, typeChip(ref), Nil, doc))),
        )

  /** For a sum-typed body, map each case name to the ref of its sub-type (so per-case rows can link). */
  private def caseRefsByName(full: FullCompiledApiSpec, body: RawCompiledResponseBody): Map[String, CompiledSchemaRef] = {
    val bodyRef: Option[CompiledSchemaRef] = body match
      case RawCompiledResponseBody.Empty               => None
      case RawCompiledResponseBody.Single(ref)         => Some(ref)
      case RawCompiledResponseBody.ServerSentEvents(r) => Some(r)
      case RawCompiledResponseBody.LineStream(r)       => Some(r)
    bodyRef.fold(Map.empty) { ref =>
      unwrapTransform(full.resolve(ref)) match
        case sum: FullCompiledJsonSchema.JsonSum => sum.cases.map(c => c.caseName -> c.caseType.value.ref).toMap
        case _                                   => Map.empty
    }
  }

  private def responseBlock(full: FullCompiledApiSpec, label: String, resp: RawCompiledResponse): Widget = {
    val bodyWidget: Widget = resp.body match
      case RawCompiledResponseBody.Empty               => span("<empty>", color := S.color.fg.subtle, fontSize := 13.px)
      case RawCompiledResponseBody.Single(ref)         => typeChip(ref)
      case RawCompiledResponseBody.ServerSentEvents(r) => row(tagPill("SSE"), typeChip(r))
      case RawCompiledResponseBody.LineStream(r)       => row(tagPill("lines"), typeChip(r))
    // when the response is a sum type, each case maps to its own status — show that breakdown
    // (linking each case to its sub-type) instead of the aggregate badges
    val aggregateStatus: Seq[Node] = if resp.caseStatuses.isEmpty then statusNodes(resp.status) else Nil
    val caseRefs: Map[String, CompiledSchemaRef] = if resp.caseStatuses.isEmpty then Map.empty else caseRefsByName(full, resp.body)
    val perCase: Widget =
      if resp.caseStatuses.isEmpty then fragment
      else
        div(margin := css(S.spacing._3, "0", "0", "0"))(
          Widget.foreach(resp.caseStatuses.toList.sortBy(_.code)) { cs =>
            val nameWidget: Node = caseRefs.get(cs.caseName) match
              case Some(ref) => typeLink(cs.caseName, ref)
              case None      => span(cs.caseName, mono, fontSize := 13.px, color := S.color.fg.moderate)
            row(statusCodePill(cs.code), nameWidget)
          },
        )
    div(
      // status sits next to the SUCCESS / ERROR label
      div(
        display.flex,
        alignItems.center,
        gap := S.spacing._2,
        margin := css(S.spacing._3, "0", S.spacing._1, "0"),
      )((subLabelSpan(label) +: aggregateStatus)*),
      row(bodyWidget),
      perCase,
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Schema rendering
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Peel `.transform`/`.transformOrFail` layers so we render what a mapped type actually wraps. */
  @annotation.tailrec
  private def unwrapTransform(s: FullCompiledSchema): FullCompiledSchema =
    s match
      case t: FullCompiledJsonSchema.Transformed  => unwrapTransform(t.underlyingType.value)
      case t: FullCompiledPlainSchema.Transformed => unwrapTransform(t.underlyingType.value)
      case other                                  => other

  private def isStructured(s: FullCompiledSchema): Boolean =
    unwrapTransform(s) match
      case _: FullCompiledJsonSchema.JsonProduct => true
      case _: FullCompiledJsonSchema.JsonSum     => true
      case _                                     => false

  /** Products / sums first (including transform-aliases of them), then everything else. */
  private def orderedSchemas(full: FullCompiledApiSpec): List[FullCompiledSchema] = {
    val (structured, leaves) = full.schemas.allSchemas.toList.partition(isStructured)
    structured ::: leaves
  }

  private def renderSchema(schema: FullCompiledSchema): Node = {
    val actual: FullCompiledSchema = unwrapTransform(schema)
    val transformNote: Widget =
      if actual ne schema then span(s"  transform of ${actual.ref.showBase}", i, color := S.color.fg.subtle, fontSize := 12.px)
      else fragment
    val body: Widget =
      actual match {
        case p: FullCompiledJsonSchema.JsonProduct =>
          div(
            i("object", color := S.color.fg.subtle, fontSize := 12.px),
            attrTable(p.fields.map(f => Attr(f.fieldName, typeChip(f.fieldType.value.ref), fieldPills(f), f.description))),
          )
        case s: FullCompiledJsonSchema.JsonSum =>
          div(
            i(s"one-of${s.discriminator.fold("")(d => s"  (discriminator: $d)")}", color := S.color.fg.subtle, fontSize := 12.px),
            attrTable(s.cases.map(c => Attr(c.caseName, typeChip(c.caseType.value.ref), Nil, None))),
          )
        case leaf =>
          div(span(leaf.ref.showCore, mono, fontSize := 13.px, color := S.color.fg.moderate))
      }
    card(
      id := anchorId(schema.ref),
      div(span(schema.ref.showBase, mono, fontWeight.bold, fontSize := 15.px, color := S.color.fg.default), transformNote),
      body,
    )
  }

}
