package oxygen.ui.web.create

import oxygen.ui.web.internal.DOMElement

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Special
//////////////////////////////////////////////////////////////////////////////////////////////////////

case object cursor extends CssBuilder("cursor") { self =>
  def alias: CSSAttr = self := "alias"
  def allScroll: CSSAttr = self := "all-scroll"
  def auto: CSSAttr = self := "auto"
  def cell: CSSAttr = self := "cell"
  def contextMenu: CSSAttr = self := "context-menu"
  def colResize: CSSAttr = self := "col-resize"
  def copy: CSSAttr = self := "copy"
  def crosshair: CSSAttr = self := "crosshair"
  def default: CSSAttr = self := "default"
  def eResize: CSSAttr = self := "e-resize"
  def ewResize: CSSAttr = self := "ew-resize"
  def grab: CSSAttr = self := "grab"
  def grabbing: CSSAttr = self := "grabbing"
  def help: CSSAttr = self := "help"
  def move: CSSAttr = self := "move"
  def nResize: CSSAttr = self := "n-resize"
  def neResize: CSSAttr = self := "ne-resize"
  def neswResize: CSSAttr = self := "nesw-resize"
  def noDrop: CSSAttr = self := "no-drop"
  def none: CSSAttr = self := "none"
  def notAllowed: CSSAttr = self := "not-allowed"
  def nsResize: CSSAttr = self := "ns-resize"
  def nwResize: CSSAttr = self := "nw-resize"
  def nwseResize: CSSAttr = self := "nwse-resize"
  def pointer: CSSAttr = self := "pointer"
  def progress: CSSAttr = self := "progress"
  def rowResize: CSSAttr = self := "row-resize"
  def sResize: CSSAttr = self := "s-resize"
  def seResize: CSSAttr = self := "se-resize"
  def swResize: CSSAttr = self := "sw-resize"
  def text: CSSAttr = self := "text"
  def verticalText: CSSAttr = self := "vertical-text"
  def wResize: CSSAttr = self := "w-resize"
  def _wait: CSSAttr = self := "wait"
  def zoomIn: CSSAttr = self := "zoom-in"
  def zoomOut: CSSAttr = self := "zoom-out"
}

case object textAlign extends CssBuilder("text-align") { self =>
  def start: CSSAttr = self := "start"
  def end: CSSAttr = self := "end"
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
  def center: CSSAttr = self := "center"
  def justify: CSSAttr = self := "justify"
  def matchParent: CSSAttr = self := "match-parent"
}

case object verticalAlign extends CssBuilder("vertical-align") { self =>
  def baseline: CSSAttr = self := "baseline"
  def sub: CSSAttr = self := "sub"
  def `super`: CSSAttr = self := "super"
  def textTop: CSSAttr = self := "text-top"
  def textBottom: CSSAttr = self := "text-bottom"
  def middle: CSSAttr = self := "middle"
  def top: CSSAttr = self := "top"
  def bottom: CSSAttr = self := "bottom"
}

case object display extends CssBuilder("display") { self =>

  def block: CSSAttr = self := "block"
  def inline: CSSAttr = self := "inline"
  def inlineBlock: CSSAttr = self := "inline-block"
  def flex: CSSAttr = self := "flex"
  def inlineFlex: CSSAttr = self := "inline-flex"
  def grid: CSSAttr = self := "grid"
  def inlineGrid: CSSAttr = self := "inline-grid"
  def flowRoot: CSSAttr = self := "flow-root"
  def none: CSSAttr = self := "none"

}

case object whiteSpace extends CssBuilder("white-space") { self =>
  def normal: CSSAttr = self := "normal"
  def pre: CSSAttr = self := "pre"
  def preWrap: CSSAttr = self := "pre-wrap"
  def preLine: CSSAttr = self := "pre-line"
  def nowrap: CSSAttr = self := "nowrap"
  def wrap: CSSAttr = self := "wrap"
  def breakSpaces: CSSAttr = self := "break-spaces"
  def collapse: CSSAttr = self := "collapse"
  def preserveNowrap: CSSAttr = self := "preserve nowrap"
}

case object position extends CssBuilder("position") { self =>
  def static: CSSAttr = self := "static"
  def relative: CSSAttr = self := "relative"
  def absolute: CSSAttr = self := "absolute"
  def fixed: CSSAttr = self := "fixed"
  def sticky: CSSAttr = self := "sticky"
}

case object visibility extends CssBuilder("visibility") { self =>
  def visible: CSSAttr = self := "visible"
  def hidden: CSSAttr = self := "hidden"
  def collapse: CSSAttr = self := "collapse"
}

case object userSelect extends CssBuilder("user-select") { self =>
  def auto: CSSAttr = self := "auto"
  def text: CSSAttr = self := "text"
  def none: CSSAttr = self := "none"
  def contain: CSSAttr = self := "contain"
  def all: CSSAttr = self := "all"
}

case object borderRadius extends CssBuilder("border-radius")
case object borderBottomLeftRadius extends CssBuilder("border-bottom-left-radius")
case object borderBottomRightRadius extends CssBuilder("border-bottom-right-radius")
case object borderTopLeftRadius extends CssBuilder("border-top-left-radius")
case object borderTopRightRadius extends CssBuilder("border-top-right-radius")

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Other
//////////////////////////////////////////////////////////////////////////////////////////////////////

case object width extends CssBuilder("width") { self =>
  def fitContent: CSSAttr = self := "fit-content"
}

case object height extends CssBuilder("height")
case object minWidth extends CssBuilder("min-width")
case object minHeight extends CssBuilder("min-height")
case object maxWidth extends CssBuilder("max-width")
case object maxHeight extends CssBuilder("max-height")

case object margin extends CssBuilder("margin"), CssBuilder.SpacingBuilder
case object marginTop extends CssBuilder("margin-top")
case object marginRight extends CssBuilder("margin-right")
case object marginBottom extends CssBuilder("margin-bottom")
case object marginLeft extends CssBuilder("margin-left")

case object padding extends CssBuilder("padding"), CssBuilder.SpacingBuilder
case object paddingTop extends CssBuilder("padding-top")
case object paddingRight extends CssBuilder("padding-right")
case object paddingBottom extends CssBuilder("padding-bottom")
case object paddingLeft extends CssBuilder("padding-left")

case object color extends CssBuilder("color"), CssBuilder.ColorBuilder
case object backgroundColor extends CssBuilder("background-color"), CssBuilder.ColorBuilder
case object background extends CssBuilder("background")
case object backgroundImage extends CssBuilder("background-image")
case object backgroundRepeat extends CssBuilder("background-repeat")
case object backgroundPosition extends CssBuilder("background-position")
case object backgroundSize extends CssBuilder("background-size")
case object backgroundClip extends CssBuilder("background-clip")
case object backgroundOrigin extends CssBuilder("background-origin")
case object backgroundAttachment extends CssBuilder("background-attachment")

case object fontSize extends CssBuilder("font-size")
case object fontWeight extends CssBuilder("font-weight") { self =>
  def normal: CSSAttr = self := "normal"
  def bold: CSSAttr = self := "bold"
  def bolder: CSSAttr = self := "bolder"
  def lighter: CSSAttr = self := "lighter"
  def _100: CSSAttr = self := "100"
  def _200: CSSAttr = self := "200"
  def _300: CSSAttr = self := "300"
  def _400: CSSAttr = self := "400"
  def _500: CSSAttr = self := "500"
  def _600: CSSAttr = self := "600"
  def _700: CSSAttr = self := "700"
  def _800: CSSAttr = self := "800"
  def _900: CSSAttr = self := "900"
}
case object fontFamily extends CssBuilder("font-family")
case object fontStyle extends CssBuilder("font-style") { self =>
  def normal: CSSAttr = self := "normal"
  def italic: CSSAttr = self := "italic"
  def oblique: CSSAttr = self := "oblique"
}
case object fontVariant extends CssBuilder("font-variant")
case object fontStretch extends CssBuilder("font-stretch")
case object textDecoration extends CssBuilder("text-decoration")
case object textTransform extends CssBuilder("text-transform") { self =>
  def none: CSSAttr = self := "none"
  def capitalize: CSSAttr = self := "capitalize"
  def uppercase: CSSAttr = self := "uppercase"
  def lowercase: CSSAttr = self := "lowercase"
  def fullWidth: CSSAttr = self := "full-width"
}
case object textOverflow extends CssBuilder("text-overflow")
case object lineHeight extends CssBuilder("line-height")
case object letterSpacing extends CssBuilder("letter-spacing")
case object wordSpacing extends CssBuilder("word-spacing")
case object direction extends CssBuilder("direction")
case object unicodeBidi extends CssBuilder("unicode-bidi")

case object border extends CssBuilder("border"), CssBuilder.BorderBuilder
case object borderTop extends CssBuilder("border-top"), CssBuilder.BorderBuilder
case object borderRight extends CssBuilder("border-right"), CssBuilder.BorderBuilder
case object borderBottom extends CssBuilder("border-bottom"), CssBuilder.BorderBuilder
case object borderLeft extends CssBuilder("border-left"), CssBuilder.BorderBuilder

case object borderColor extends CssBuilder("border-color"), CssBuilder.ColorBuilder
case object borderTopColor extends CssBuilder("border-top-color"), CssBuilder.ColorBuilder
case object borderRightColor extends CssBuilder("border-right-color"), CssBuilder.ColorBuilder
case object borderBottomColor extends CssBuilder("border-bottom-color"), CssBuilder.ColorBuilder
case object borderLeftColor extends CssBuilder("border-left-color"), CssBuilder.ColorBuilder

case object borderStyle extends CssBuilder("border-style"), CssBuilder.BorderStyleBuilder
case object borderTopStyle extends CssBuilder("border-top-style"), CssBuilder.BorderStyleBuilder
case object borderRightStyle extends CssBuilder("border-right-style"), CssBuilder.BorderStyleBuilder
case object borderBottomStyle extends CssBuilder("border-bottom-style"), CssBuilder.BorderStyleBuilder
case object borderLeftStyle extends CssBuilder("border-left-style"), CssBuilder.BorderStyleBuilder

case object borderWidth extends CssBuilder("border-width")
case object borderTopWidth extends CssBuilder("border-top-width")
case object borderRightWidth extends CssBuilder("border-right-width")
case object borderBottomWidth extends CssBuilder("border-bottom-width")
case object borderLeftWidth extends CssBuilder("border-left-width")

case object borderCollapse extends CssBuilder("border-collapse") { self =>
  def separate: CSSAttr = self := "separate"
  def collapse: CSSAttr = self := "collapse"
}
case object borderSpacing extends CssBuilder("border-spacing")

case object top extends CssBuilder("top")
case object right extends CssBuilder("right")
case object bottom extends CssBuilder("bottom")
case object left extends CssBuilder("left")
case object zIndex extends CssBuilder("z-index")
case object float extends CssBuilder("float") { self =>
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
  def none: CSSAttr = self := "none"
  def inlineStart: CSSAttr = self := "inline-start"
  def inlineEnd: CSSAttr = self := "inline-end"
}
case object clear extends CssBuilder("clear") { self =>
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
  def both: CSSAttr = self := "both"
  def none: CSSAttr = self := "none"
  def inlineStart: CSSAttr = self := "inline-start"
  def inlineEnd: CSSAttr = self := "inline-end"
}
case object overflow extends CssBuilder("overflow"), CssBuilder.OverflowBuilder
case object overflowX extends CssBuilder("overflow-x"), CssBuilder.OverflowBuilder
case object overflowY extends CssBuilder("overflow-y"), CssBuilder.OverflowBuilder
case object opacity extends CssBuilder("opacity")
case object boxShadow extends CssBuilder("box-shadow")
case object outline extends CssBuilder("outline")
case object outlineColor extends CssBuilder("outline-color")
case object outlineStyle extends CssBuilder("outline-style")
case object outlineWidth extends CssBuilder("outline-width")
case object transition extends CssBuilder("transition")
case object transform extends CssBuilder("transform")
case object filter extends CssBuilder("filter")
case object clipPath extends CssBuilder("clip-path")
case object content extends CssBuilder("content")
case object order extends CssBuilder("order")

// Flexbox
case object flex extends CssBuilder("flex")
case object flexBasis extends CssBuilder("flex-basis")
case object flexDirection extends CssBuilder("flex-direction") { self =>
  def row: CSSAttr = self := "row"
  def rowReverse: CSSAttr = self := "row-reverse"
  def column: CSSAttr = self := "column"
  def columnReverse: CSSAttr = self := "column-reverse"
}
case object flexFlow extends CssBuilder("flex-flow")
case object flexGrow extends CssBuilder("flex-grow")
case object flexShrink extends CssBuilder("flex-shrink")
case object flexWrap extends CssBuilder("flex-wrap") { self =>
  def nowrap: CSSAttr = self := "nowrap"
  def wrap: CSSAttr = self := "wrap"
  def wrapReverse: CSSAttr = self := "wrap-reverse"
}
case object alignItems extends CssBuilder("align-items") { self =>
  def stretch: CSSAttr = self := "stretch"
  def center: CSSAttr = self := "center"
  def flexStart: CSSAttr = self := "flex-start"
  def flexEnd: CSSAttr = self := "flex-end"
  def baseline: CSSAttr = self := "baseline"
}
case object alignContent extends CssBuilder("align-content")
case object alignSelf extends CssBuilder("align-self")
case object justifyContent extends CssBuilder("justify-content") { self =>
  def flexStart: CSSAttr = self := "flex-start"
  def flexEnd: CSSAttr = self := "flex-end"
  def center: CSSAttr = self := "center"
  def spaceBetween: CSSAttr = self := "space-between"
  def spaceAround: CSSAttr = self := "space-around"
  def spaceEvenly: CSSAttr = self := "space-evenly"
}
case object justifyItems extends CssBuilder("justify-items") { self =>
  def stretch: CSSAttr = self := "stretch"
  def center: CSSAttr = self := "center"
  def start: CSSAttr = self := "start"
  def end: CSSAttr = self := "end"
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
}
case object justifySelf extends CssBuilder("justify-self") { self =>
  def stretch: CSSAttr = self := "stretch"
  def center: CSSAttr = self := "center"
  def start: CSSAttr = self := "start"
  def end: CSSAttr = self := "end"
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
}
case object gap extends CssBuilder("gap")
case object rowGap extends CssBuilder("row-gap")
case object columnGap extends CssBuilder("column-gap")

// Grid
case object grid extends CssBuilder("grid")
case object gridArea extends CssBuilder("grid-area")
case object gridTemplate extends CssBuilder("grid-template")
case object gridTemplateAreas extends CssBuilder("grid-template-areas")
case object gridTemplateColumns extends CssBuilder("grid-template-columns")
case object gridTemplateRows extends CssBuilder("grid-template-rows")
case object gridColumn extends CssBuilder("grid-column")
case object gridRow extends CssBuilder("grid-row")
case object gridColumnStart extends CssBuilder("grid-column-start")
case object gridColumnEnd extends CssBuilder("grid-column-end")
case object gridRowStart extends CssBuilder("grid-row-start")
case object gridRowEnd extends CssBuilder("grid-row-end")
case object gridAutoColumns extends CssBuilder("grid-auto-columns")
case object gridAutoRows extends CssBuilder("grid-auto-rows")
case object gridAutoFlow extends CssBuilder("grid-auto-flow")

// Misc
case object tabIndex extends CssBuilder("tab-index")
case object listStyle extends CssBuilder("list-style")
case object listStyleType extends CssBuilder("list-style-type")
case object listStylePosition extends CssBuilder("list-style-position")
case object listStyleImage extends CssBuilder("list-style-image")
case object quotes extends CssBuilder("quotes")
case object counterReset extends CssBuilder("counter-reset")
case object counterIncrement extends CssBuilder("counter-increment")
case object willChange extends CssBuilder("will-change")
case object objectPosition extends CssBuilder("object-position")
case object wordBreak extends CssBuilder("word-break")
case object wordWrap extends CssBuilder("word-wrap")
case object hyphens extends CssBuilder("hyphens")
case object caretColor extends CssBuilder("caret-color")
case object scrollBehavior extends CssBuilder("scroll-behavior")
case object scrollSnapType extends CssBuilder("scroll-snap-type")
case object scrollSnapAlign extends CssBuilder("scroll-snap-align")
case object scrollMargin extends CssBuilder("scroll-margin")
case object scrollPadding extends CssBuilder("scroll-padding")
case object touchAction extends CssBuilder("touch-action")
case object accentColor extends CssBuilder("accent-color")
case object aspectRatio extends CssBuilder("aspect-ratio")
case object inset extends CssBuilder("inset")
case object insetBlock extends CssBuilder("inset-block")
case object insetInline extends CssBuilder("inset-inline")
case object insetBlockStart extends CssBuilder("inset-block-start")
case object insetBlockEnd extends CssBuilder("inset-block-end")
case object insetInlineStart extends CssBuilder("inset-inline-start")
case object insetInlineEnd extends CssBuilder("inset-inline-end")

case object boxSizing extends CssBuilder("box-sizing") { self =>
  def contentBox: CSSAttr = self := "content-box"
  def borderBox: CSSAttr = self := "border-box"
}
case object pointerEvents extends CssBuilder("pointer-events") { self =>
  def auto: CSSAttr = self := "auto"
  def none: CSSAttr = self := "none"
  def visible: CSSAttr = self := "visible"
  def visibleFill: CSSAttr = self := "visibleFill"
  def visibleStroke: CSSAttr = self := "visibleStroke"
  def visiblePainted: CSSAttr = self := "visiblePainted"
  def painted: CSSAttr = self := "painted"
  def fill: CSSAttr = self := "fill"
  def stroke: CSSAttr = self := "stroke"
  def all: CSSAttr = self := "all"
}
case object resize extends CssBuilder("resize") { self =>
  def none: CSSAttr = self := "none"
  def both: CSSAttr = self := "both"
  def horizontal: CSSAttr = self := "horizontal"
  def vertical: CSSAttr = self := "vertical"
  def block: CSSAttr = self := "block"
  def inline: CSSAttr = self := "inline"
}
case object objectFit extends CssBuilder("object-fit") { self =>
  def fill: CSSAttr = self := "fill"
  def contain: CSSAttr = self := "contain"
  def cover: CSSAttr = self := "cover"
  def none: CSSAttr = self := "none"
  def scaleDown: CSSAttr = self := "scale-down"
}
case object isolation extends CssBuilder("isolation") { self =>
  def auto: CSSAttr = self := "auto"
  def isolate: CSSAttr = self := "isolate"
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class CssBuilder(private[web] val key: String) { self =>

  final def :=(value: => String): CSSAttr = Widget.raw.css(key, value)
  final def :=(value: Int): CSSAttr = self := value.toString

  def csss(strs: => String*): CSSAttr = self := strs.mkString(" ")

  final def inherit: CSSAttr = self := "inherit"
  final def initial: CSSAttr = self := "initial"
  final def revert: CSSAttr = self := "revert"
  final def revertLayer: CSSAttr = self := "revert-layer"
  final def unset: CSSAttr = self := "unset"

  /**
    * Removes any previously set inline values.
    * Imagine you have a component which sets a `borderColor := "red"`,
    * and you really want to do `fragment(borderColor.dynamic := "red", borderColor.dynamic.hover := "green")`.
    * Because `borderColor := "red"` is a const inline style, it will override any of the dynamic styling capabilities.
    *
    * imagine you had:
    *
    * ```
    * val myComponent: Widget = div(borderColor := "red")
    * ```
    *
    * you could do:
    *
    * ```
    * myComponent(borderColor.removeInline, borderColor.dynamic := "red", borderColor.dynamic.hover := "green")
    * ```
    *
    * this would remove the const borderColor
    */
  final def removeInline: CSSAttr = self := DOMElement.removeInlineValue

  abstract class InlinePseudoAttr(pseudoSuffix: String, varSuffix: String) {

    private[web] final val baseName: String = s"inline-pseudo--$key--$varSuffix"
    private[web] final val varName: String = s"--$baseName---dynamic"
    private[web] final val cssClassName: String = s".$baseName$pseudoSuffix"

    final def :=(value: String): Widget =
      fragment(
        Widget.raw.`class`(baseName),
        Widget.raw.css(varName, value),
      )

    final def csss(strs: String*): Widget =
      this := strs.mkString(" ")

  }

  object dynamic extends InlinePseudoAttr("", "base") {
    // TODO (KR) : figure out a composable way to do this
    object hover extends InlinePseudoAttr(":hover", "hover")
    object active extends InlinePseudoAttr(":active", "active")
    object hoverActive extends InlinePseudoAttr(":hover:active", "hover-active")
    object focus extends InlinePseudoAttr(":focus", "focus")
    object hoverFocus extends InlinePseudoAttr(":hover:focus", "hover-focus")
  }

}
object CssBuilder {

  def apply(key: String): CssBuilder = Custom(key)

  private final case class Custom(k: String) extends CssBuilder(k)

  trait ColorBuilder { self: CssBuilder =>
    final def red: CSSAttr = self := "red"
    final def blue: CSSAttr = self := "blue"
    final def green: CSSAttr = self := "green"
    final def black: CSSAttr = self := "black"
    final def white: CSSAttr = self := "white"
    final def gray: CSSAttr = self := "gray"
    final def yellow: CSSAttr = self := "yellow"
    final def orange: CSSAttr = self := "orange"
    final def purple: CSSAttr = self := "purple"
    final def brown: CSSAttr = self := "brown"
    final def pink: CSSAttr = self := "pink"
    final def cyan: CSSAttr = self := "cyan"
    final def magenta: CSSAttr = self := "magenta"
    final def transparent: CSSAttr = self := "transparent"
    final def silver: CSSAttr = self := "silver"
    final def lime: CSSAttr = self := "lime"
    final def maroon: CSSAttr = self := "maroon"
    final def olive: CSSAttr = self := "olive"
    final def teal: CSSAttr = self := "teal"
    final def navy: CSSAttr = self := "navy"
    final def fuchsia: CSSAttr = self := "fuchsia"
    final def aqua: CSSAttr = self := "aqua"
    final def rgb(r: Int, g: Int, b: Int): CSSAttr = self := s"rgb($r, $g, $b)"
  }

  trait BorderStyleBuilder { self: CssBuilder =>
    def none: CSSAttr = self := "none"
    def hidden: CSSAttr = self := "hidden"
    def dotted: CSSAttr = self := "dotted"
    def dashed: CSSAttr = self := "dashed"
    def solid: CSSAttr = self := "solid"
    def double: CSSAttr = self := "double"
    def groove: CSSAttr = self := "groove"
    def ridge: CSSAttr = self := "ridge"
    def inset: CSSAttr = self := "inset"
    def outset: CSSAttr = self := "outset"
  }

  trait SpacingBuilder { self: CssBuilder =>
    final def apply(allSides: String): CSSAttr = self := allSides
    final def apply(topBottom: String, leftRight: String): CSSAttr = self := s"$topBottom $leftRight"
    final def apply(top: String, leftRight: String, bottom: String): CSSAttr = self := s"$top $leftRight $bottom"
    final def apply(top: String, right: String, bottom: String, left: String): CSSAttr = self := s"$top $right $bottom $left"
  }

  trait OverflowBuilder { self: CssBuilder =>
    def visible: CSSAttr = self := "visible"
    def hidden: CSSAttr = self := "hidden"
    def scroll: CSSAttr = self := "scroll"
    def auto: CSSAttr = self := "auto"
    def clip: CSSAttr = self := "clip"
  }

  trait BorderBuilder { self: CssBuilder =>

    def apply(width: String, style: String, color: String): CSSAttr = self.csss(width, style, color)
    def apply(width: String, color: String): CSSAttr = self.csss(width, "solid", color)

  }

}
