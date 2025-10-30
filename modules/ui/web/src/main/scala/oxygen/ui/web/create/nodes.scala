package oxygen.ui.web.create

import org.scalajs.dom.CanvasRenderingContext2D

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Fragment
//////////////////////////////////////////////////////////////////////////////////////////////////////

val fragment: Fragment = Widget.fragment

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Node
//////////////////////////////////////////////////////////////////////////////////////////////////////

val a: Node = Node("a")
val abbr: Node = Node("abbr")
val address: Node = Node("address")
val area: Node = Node("area")
val article: Node = Node("article")
val aside: Node = Node("aside")
val audio: Node = Node("audio")
val b: Node = Node("b")
val base: Node = Node("base")
val bdi: Node = Node("bdi")
val bdo: Node = Node("bdo")
val blockquote: Node = Node("blockquote")
val br: Node = Node("br")
val button: Node = Node("button")
val caption: Node = Node("caption")
val cite: Node = Node("cite")
val code: Node = Node("code")
val col: Node = Node("col")
val colgroup: Node = Node("colgroup")
val data: Node = Node("data")
val datalist: Node = Node("datalist")
val dd: Node = Node("dd")
val del: Node = Node("del")
val details: Node = Node("details")
val dfn: Node = Node("dfn")
val dialog: Node = Node("dialog")
val div: Node = Node("div")
val dl: Node = Node("dl")
val dt: Node = Node("dt")
val embed: Node = Node("embed")
val fieldset: Node = Node("fieldset")
val figcaption: Node = Node("figcaption")
val figure: Node = Node("figure")
val footer: Node = Node("footer")
val form: Node = Node("form")
val h1: Node = Node("h1")
val h2: Node = Node("h2")
val h3: Node = Node("h3")
val h4: Node = Node("h4")
val h5: Node = Node("h5")
val h6: Node = Node("h6")
val head: Node = Node("head")
val header: Node = Node("header")
val hgroup: Node = Node("hgroup")
val hr: Node = Node("hr")
val i: Node = Node("i")
val iframe: Node = Node("iframe")
val img: Node = Node("img")
val input: Node = Node("input")
val ins: Node = Node("ins")
val kbd: Node = Node("kbd")
val label: Node = Node("label")
val legend: Node = Node("legend")
val li: Node = Node("li")
val link: Node = Node("link")
val main: Node = Node("main")
val map: Node = Node("map")
val mark: Node = Node("mark")
val menu: Node = Node("menu")
val meta: Node = Node("meta")
val meter: Node = Node("meter")
val nav: Node = Node("nav")
val noscript: Node = Node("noscript")
val objectTag: Node = Node("object")
val ol: Node = Node("ol")
val optgroup: Node = Node("optgroup")
val option: Node = Node("option")
val output: Node = Node("output")
val p: Node = Node("p")
val param: Node = Node("param")
val picture: Node = Node("picture")
val pre: Node = Node("pre")
val progress: Node = Node("progress")
val q: Node = Node("q")
val rp: Node = Node("rp")
val rt: Node = Node("rt")
val ruby: Node = Node("ruby")
val s: Node = Node("s")
val samp: Node = Node("samp")
val script: Node = Node("script")
val section: Node = Node("section")
val select: Node = Node("select")
val slot: Node = Node("slot")
val small: Node = Node("small")
val source: Node = Node("source")
val span: Node = Node("span")
val strong: Node = Node("strong")
val style: Node = Node("style")
val sub: Node = Node("sub")
val summary: Node = Node("summary")
val sup: Node = Node("sup")
val table: Node = Node("table")
val tbody: Node = Node("tbody")
val td: Node = Node("td")
val template: Node = Node("template")
val textArea: Node = Node("textarea")
val tfoot: Node = Node("tfoot")
val th: Node = Node("th")
val thead: Node = Node("thead")
val time: Node = Node("time")
val title: Node = Node("title")
val tr: Node = Node("tr")
val track: Node = Node("track")
val u: Node = Node("u")
val ul: Node = Node("ul")
val `var`: Node = Node("var")
val video: Node = Node("video")
val wbr: Node = Node("wbr")

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Canvas
//////////////////////////////////////////////////////////////////////////////////////////////////////

def canvas(draw: CanvasRenderingContext2D => Unit): Canvas = Canvas(draw)

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SVG
//////////////////////////////////////////////////////////////////////////////////////////////////////

val svg: Node = Node("http://www.w3.org/2000/svg", "svg")
val svgG: Node = Node("http://www.w3.org/2000/svg", "g")
val svgPath: Node = Node("http://www.w3.org/2000/svg", "path")
val svgPolygon: Node = Node("http://www.w3.org/2000/svg", "polygon")

def basicSvg(w: Int, h: Int): Node =
  svg(
    htmlWidth := w,
    htmlHeight := h,
    svgViewBox := s"0 0 $w $h",
  )
