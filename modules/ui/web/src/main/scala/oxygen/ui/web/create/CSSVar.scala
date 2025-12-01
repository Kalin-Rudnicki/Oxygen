package oxygen.ui.web.create

import org.scalajs.dom.{document, window}

final class CSSVar(baseName: String) {

  val name: String = s"--$baseName"
  val varString: String = s"var($name)"

  def getValue: String = CSSVar.getValue(name)
  def getColorValue: CSSColor = CSSColor(this)

  def :=(value: String): CSSAttr = Widget.raw.css(name, value)
  def csss(strs: String*): CSSAttr = this := strs.mkString(" ")

  override def toString: String = varString

}
object CSSVar {

  // TODO (KR) : This might need special representation within `Widget`, so that this can be re-evaluated.
  //           : This might come into play when the time for dynamic light/dark mode arrives.
  //           : enum CSSPropertyValue { case Const(value: String); case Dynamic(eval: () => String) }
  def getValue(name: String): String = {
    val res = window.getComputedStyle(document.documentElement).getPropertyValue(name).trim

    if res.isEmpty then
      throw new RuntimeException(
        s"""Attempted to read css-var '$name', but the value was empty.
           |This is most likely because you directly or indirectly called `CSSVar.eval`/`CSSVar.getValue` from a `val`.
           |This is an issue, because the `val` is evaluated before the page loads, and the CSS values are added to the DOM.
           |Using a `lazy val` or a `def` should resolve this.""".stripMargin,
      )

    res
  }

  def getValueAllowEmpty(name: String): String =
    window.getComputedStyle(document.documentElement).getPropertyValue(name).trim

  private val varReg = "^var\\((--[A-Za-z0-9_\\-]+)\\)$".r

  def eval(str: String): String = str match
    case varReg(name) => getValue(name)
    case _            => str

}
