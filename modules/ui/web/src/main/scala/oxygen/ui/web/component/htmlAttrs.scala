package oxygen.ui.web.component

import oxygen.ui.web.Widget

object `type` extends TypedHtmlAttrBuilder[String]("type") { self =>
  inline def text: Widget.Raw.HtmlAttr = self := "text"
  inline def number: Widget.Raw.HtmlAttr = self := "number"
  inline def password: Widget.Raw.HtmlAttr = self := "password"
  inline def checkbox: Widget.Raw.HtmlAttr = self := "checkbox"
  inline def radio: Widget.Raw.HtmlAttr = self := "radio"
  inline def submit: Widget.Raw.HtmlAttr = self := "submit"
  inline def reset: Widget.Raw.HtmlAttr = self := "reset"
  inline def button: Widget.Raw.HtmlAttr = self := "button"
  inline def hidden: Widget.Raw.HtmlAttr = self := "hidden"
  inline def image: Widget.Raw.HtmlAttr = self := "image"
  inline def file: Widget.Raw.HtmlAttr = self := "file"
  inline def color: Widget.Raw.HtmlAttr = self := "color"
  inline def date: Widget.Raw.HtmlAttr = self := "date"
  inline def datetime: Widget.Raw.HtmlAttr = self := "datetime"
  inline def datetimeLocal: Widget.Raw.HtmlAttr = self := "datetime-local"
  inline def email: Widget.Raw.HtmlAttr = self := "email"
  inline def month: Widget.Raw.HtmlAttr = self := "month"
  inline def range: Widget.Raw.HtmlAttr = self := "range"
  inline def search: Widget.Raw.HtmlAttr = self := "search"
  inline def tel: Widget.Raw.HtmlAttr = self := "tel"
  inline def time: Widget.Raw.HtmlAttr = self := "time"
  inline def url: Widget.Raw.HtmlAttr = self := "url"
  inline def week: Widget.Raw.HtmlAttr = self := "week"
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

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

abstract class TypedHtmlAttrBuilder[T](key: String, convert: T => String = (_: T).toString) { self =>
  final def :=(value: T): Widget.Raw.HtmlAttr = Widget.Raw.attr(key, convert(value))
  final def empty: Widget.Raw.HtmlAttr = Widget.Raw.attr(key, "")
}
