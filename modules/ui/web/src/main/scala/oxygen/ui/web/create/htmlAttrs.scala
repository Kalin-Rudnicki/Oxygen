package oxygen.ui.web.create

object `type` extends TypedHtmlAttrBuilder[String]("type") { self =>
  inline def text: HtmlAttr = self := "text"
  inline def number: HtmlAttr = self := "number"
  inline def password: HtmlAttr = self := "password"
  inline def checkbox: HtmlAttr = self := "checkbox"
  inline def radio: HtmlAttr = self := "radio"
  inline def submit: HtmlAttr = self := "submit"
  inline def reset: HtmlAttr = self := "reset"
  inline def button: HtmlAttr = self := "button"
  inline def hidden: HtmlAttr = self := "hidden"
  inline def image: HtmlAttr = self := "image"
  inline def file: HtmlAttr = self := "file"
  inline def color: HtmlAttr = self := "color"
  inline def date: HtmlAttr = self := "date"
  inline def datetime: HtmlAttr = self := "datetime"
  inline def datetimeLocal: HtmlAttr = self := "datetime-local"
  inline def email: HtmlAttr = self := "email"
  inline def month: HtmlAttr = self := "month"
  inline def range: HtmlAttr = self := "range"
  inline def search: HtmlAttr = self := "search"
  inline def tel: HtmlAttr = self := "tel"
  inline def time: HtmlAttr = self := "time"
  inline def url: HtmlAttr = self := "url"
  inline def week: HtmlAttr = self := "week"
}

object `for` extends TypedHtmlAttrBuilder[String]("for")
object id extends TypedHtmlAttrBuilder[String]("id")
object src extends TypedHtmlAttrBuilder[String]("src")

object rowSpan extends TypedHtmlAttrBuilder[Int]("rowSpan")
object colSpan extends TypedHtmlAttrBuilder[Int]("colSpan")

object multiple extends TypedHtmlAttrBuilder[String]("multiple")
object controls extends TypedHtmlAttrBuilder[String]("controls")
object autoplay extends TypedHtmlAttrBuilder[String]("autoplay")

object htmlWidth extends TypedHtmlAttrBuilder[Int]("width")
object htmlHeight extends TypedHtmlAttrBuilder[Int]("height")

object href extends TypedHtmlAttrBuilder[String]("href")

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SVG
//////////////////////////////////////////////////////////////////////////////////////////////////////

object svgViewBox extends TypedHtmlAttrBuilder[String]("viewBox")
object svgFill extends TypedHtmlAttrBuilder[String]("fill") { self =>
  def none: HtmlAttr = self := "none"
}
object svgStroke extends TypedHtmlAttrBuilder[String]("stroke") { self =>
  def currentColor: HtmlAttr = self := "currentColor"
  // TODO (KR) : this is not a css attr, so it cant extend that builder, but add color helpers here
}
object svgStrokeWidth extends TypedHtmlAttrBuilder[Int]("stroke-width")
object svgStrokeLineCap extends TypedHtmlAttrBuilder[String]("stroke-linecap") { self =>
  def round: HtmlAttr = self := "round"
}
object svgStrokeLineJoin extends TypedHtmlAttrBuilder[String]("stroke-linejoin") { self =>
  def round: HtmlAttr = self := "round"
}
object svgPoints extends TypedHtmlAttrBuilder[String]("points")
object svgD extends TypedHtmlAttrBuilder[String]("d")

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

abstract class TypedHtmlAttrBuilder[T](key: String, convert: T => String = (_: T).toString) { self =>
  final def :=(value: T): HtmlAttr = Widget.raw.htmlAttr(key, convert(value))
  final def empty: HtmlAttr = Widget.raw.htmlAttr(key, "")
}
