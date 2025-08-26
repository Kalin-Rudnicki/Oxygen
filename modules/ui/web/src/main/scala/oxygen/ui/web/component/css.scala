package oxygen.ui.web.component

import oxygen.ui.web.Widget

val ab = org.scalajs.dom.CanvasRenderingContext2D()

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Special
//////////////////////////////////////////////////////////////////////////////////////////////////////

object cursor extends CssBuilder("cursor") { self =>
  def alias: Widget.Raw.CSSAttr = self := "alias"
  def allScroll: Widget.Raw.CSSAttr = self := "all-scroll"
  def auto: Widget.Raw.CSSAttr = self := "auto"
  def cell: Widget.Raw.CSSAttr = self := "cell"
  def contextMenu: Widget.Raw.CSSAttr = self := "context-menu"
  def colResize: Widget.Raw.CSSAttr = self := "col-resize"
  def copy: Widget.Raw.CSSAttr = self := "copy"
  def crosshair: Widget.Raw.CSSAttr = self := "crosshair"
  def default: Widget.Raw.CSSAttr = self := "default"
  def eResize: Widget.Raw.CSSAttr = self := "e-resize"
  def ewResize: Widget.Raw.CSSAttr = self := "ew-resize"
  def grab: Widget.Raw.CSSAttr = self := "grab"
  def grabbing: Widget.Raw.CSSAttr = self := "grabbing"
  def help: Widget.Raw.CSSAttr = self := "help"
  def move: Widget.Raw.CSSAttr = self := "move"
  def nResize: Widget.Raw.CSSAttr = self := "n-resize"
  def neResize: Widget.Raw.CSSAttr = self := "ne-resize"
  def neswResize: Widget.Raw.CSSAttr = self := "nesw-resize"
  def noDrop: Widget.Raw.CSSAttr = self := "no-drop"
  def none: Widget.Raw.CSSAttr = self := "none"
  def notAllowed: Widget.Raw.CSSAttr = self := "not-allowed"
  def nsResize: Widget.Raw.CSSAttr = self := "ns-resize"
  def nwResize: Widget.Raw.CSSAttr = self := "nw-resize"
  def nwseResize: Widget.Raw.CSSAttr = self := "nwse-resize"
  def pointer: Widget.Raw.CSSAttr = self := "pointer"
  def progress: Widget.Raw.CSSAttr = self := "progress"
  def rowResize: Widget.Raw.CSSAttr = self := "row-resize"
  def sResize: Widget.Raw.CSSAttr = self := "s-resize"
  def seResize: Widget.Raw.CSSAttr = self := "se-resize"
  def swResize: Widget.Raw.CSSAttr = self := "sw-resize"
  def text: Widget.Raw.CSSAttr = self := "text"
  def verticalText: Widget.Raw.CSSAttr = self := "vertical-text"
  def wResize: Widget.Raw.CSSAttr = self := "w-resize"
  def _wait: Widget.Raw.CSSAttr = self := "wait"
  def zoomIn: Widget.Raw.CSSAttr = self := "zoom-in"
  def zoomOut: Widget.Raw.CSSAttr = self := "zoom-out"
}

object textAlign extends CssBuilder("text-align") { self =>
  def start: Widget.Raw.CSSAttr = self := "start"
  def end: Widget.Raw.CSSAttr = self := "end"
  def left: Widget.Raw.CSSAttr = self := "left"
  def right: Widget.Raw.CSSAttr = self := "right"
  def center: Widget.Raw.CSSAttr = self := "center"
  def justify: Widget.Raw.CSSAttr = self := "justify"
  def matchParent: Widget.Raw.CSSAttr = self := "match-parent"
}

object display extends CssBuilder("display") { self =>

  def block: Widget.Raw.CSSAttr = self := "block"
  def inline: Widget.Raw.CSSAttr = self := "inline"
  def inlineBlock: Widget.Raw.CSSAttr = self := "inline-block"
  def flex: Widget.Raw.CSSAttr = self := "flex"
  def inlineFlex: Widget.Raw.CSSAttr = self := "inline-flex"
  def grid: Widget.Raw.CSSAttr = self := "grid"
  def inlineGrid: Widget.Raw.CSSAttr = self := "inline-grid"
  def flowRoot: Widget.Raw.CSSAttr = self := "flow-root"

}

object whiteSpace extends CssBuilder("white-space") { self =>
  def normal: Widget.Raw.CSSAttr = self := "normal"
  def pre: Widget.Raw.CSSAttr = self := "pre"
  def preWrap: Widget.Raw.CSSAttr = self := "pre-wrap"
  def preLine: Widget.Raw.CSSAttr = self := "pre-line"
  def nowrap: Widget.Raw.CSSAttr = self := "nowrap"
  def wrap: Widget.Raw.CSSAttr = self := "wrap"
  def breakSpaces: Widget.Raw.CSSAttr = self := "break-spaces"
  def collapse: Widget.Raw.CSSAttr = self := "collapse"
  def preserveNowrap: Widget.Raw.CSSAttr = self := "preserve nowrap"
}

object position extends CssBuilder("position") { self =>
  def static: Widget.Raw.CSSAttr = self := "static"
  def relative: Widget.Raw.CSSAttr = self := "relative"
  def absolute: Widget.Raw.CSSAttr = self := "absolute"
  def fixed: Widget.Raw.CSSAttr = self := "fixed"
  def sticky: Widget.Raw.CSSAttr = self := "sticky"
}

object visibility extends CssBuilder("visibility") { self =>
  def visible: Widget.Raw.CSSAttr = self := "visible"
  def hidden: Widget.Raw.CSSAttr = self := "hidden"
  def collapse: Widget.Raw.CSSAttr = self := "collapse"
}

object userSelect extends CssBuilder("user-select") { self =>
  def auto: Widget.Raw.CSSAttr = self := "auto"
  def text: Widget.Raw.CSSAttr = self := "text"
  def none: Widget.Raw.CSSAttr = self := "none"
  def contain: Widget.Raw.CSSAttr = self := "contain"
  def all: Widget.Raw.CSSAttr = self := "all"
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Other
//////////////////////////////////////////////////////////////////////////////////////////////////////

object width extends CssBuilder("width")
object height extends CssBuilder("height")
object minWidth extends CssBuilder("min-width")
object minHeight extends CssBuilder("min-height")
object maxWidth extends CssBuilder("max-width")
object maxHeight extends CssBuilder("max-height")

object margin extends CssBuilder("margin"), CssBuilder.SpacingBuilder
object marginTop extends CssBuilder("margin-top")
object marginRight extends CssBuilder("margin-right")
object marginBottom extends CssBuilder("margin-bottom")
object marginLeft extends CssBuilder("margin-left")

object padding extends CssBuilder("padding"), CssBuilder.SpacingBuilder
object paddingTop extends CssBuilder("padding-top")
object paddingRight extends CssBuilder("padding-right")
object paddingBottom extends CssBuilder("padding-bottom")
object paddingLeft extends CssBuilder("padding-left")

object color extends CssBuilder("color"), CssBuilder.ColorBuilder
object backgroundColor extends CssBuilder("background-color"), CssBuilder.ColorBuilder
object background extends CssBuilder("background")
object backgroundImage extends CssBuilder("background-image")
object backgroundRepeat extends CssBuilder("background-repeat")
object backgroundPosition extends CssBuilder("background-position")
object backgroundSize extends CssBuilder("background-size")
object backgroundClip extends CssBuilder("background-clip")
object backgroundOrigin extends CssBuilder("background-origin")
object backgroundAttachment extends CssBuilder("background-attachment")

object fontSize extends CssBuilder("font-size")
object fontWeight extends CssBuilder("font-weight") { self =>
  def normal: Widget.Raw.CSSAttr = self := "normal"
  def bold: Widget.Raw.CSSAttr = self := "bold"
  def bolder: Widget.Raw.CSSAttr = self := "bolder"
  def lighter: Widget.Raw.CSSAttr = self := "lighter"
  def w100: Widget.Raw.CSSAttr = self := "100"
  def w200: Widget.Raw.CSSAttr = self := "200"
  def w300: Widget.Raw.CSSAttr = self := "300"
  def w400: Widget.Raw.CSSAttr = self := "400"
  def w500: Widget.Raw.CSSAttr = self := "500"
  def w600: Widget.Raw.CSSAttr = self := "600"
  def w700: Widget.Raw.CSSAttr = self := "700"
  def w800: Widget.Raw.CSSAttr = self := "800"
  def w900: Widget.Raw.CSSAttr = self := "900"
}
object fontFamily extends CssBuilder("font-family")
object fontStyle extends CssBuilder("font-style") { self =>
  def normal: Widget.Raw.CSSAttr = self := "normal"
  def italic: Widget.Raw.CSSAttr = self := "italic"
  def oblique: Widget.Raw.CSSAttr = self := "oblique"
}
object fontVariant extends CssBuilder("font-variant")
object fontStretch extends CssBuilder("font-stretch")
object textDecoration extends CssBuilder("text-decoration")
object textTransform extends CssBuilder("text-transform") { self =>
  def none: Widget.Raw.CSSAttr = self := "none"
  def capitalize: Widget.Raw.CSSAttr = self := "capitalize"
  def uppercase: Widget.Raw.CSSAttr = self := "uppercase"
  def lowercase: Widget.Raw.CSSAttr = self := "lowercase"
  def fullWidth: Widget.Raw.CSSAttr = self := "full-width"
}
object textOverflow extends CssBuilder("text-overflow")
object lineHeight extends CssBuilder("line-height")
object letterSpacing extends CssBuilder("letter-spacing")
object wordSpacing extends CssBuilder("word-spacing")
object direction extends CssBuilder("direction")
object unicodeBidi extends CssBuilder("unicode-bidi")

object border extends CssBuilder("border")
object borderTop extends CssBuilder("border-top")
object borderRight extends CssBuilder("border-right")
object borderBottom extends CssBuilder("border-bottom")
object borderLeft extends CssBuilder("border-left")
object borderRadius extends CssBuilder("border-radius")
object borderColor extends CssBuilder("border-color")
object borderStyle extends CssBuilder("border-style") { self =>
  def none: Widget.Raw.CSSAttr = self := "none"
  def hidden: Widget.Raw.CSSAttr = self := "hidden"
  def dotted: Widget.Raw.CSSAttr = self := "dotted"
  def dashed: Widget.Raw.CSSAttr = self := "dashed"
  def solid: Widget.Raw.CSSAttr = self := "solid"
  def double: Widget.Raw.CSSAttr = self := "double"
  def groove: Widget.Raw.CSSAttr = self := "groove"
  def ridge: Widget.Raw.CSSAttr = self := "ridge"
  def inset: Widget.Raw.CSSAttr = self := "inset"
  def outset: Widget.Raw.CSSAttr = self := "outset"
}
object borderWidth extends CssBuilder("border-width")
object borderCollapse extends CssBuilder("border-collapse") { self =>
  def separate: Widget.Raw.CSSAttr = self := "separate"
  def collapse: Widget.Raw.CSSAttr = self := "collapse"
}
object borderSpacing extends CssBuilder("border-spacing")

object top extends CssBuilder("top")
object right extends CssBuilder("right")
object bottom extends CssBuilder("bottom")
object left extends CssBuilder("left")
object zIndex extends CssBuilder("z-index")
object float extends CssBuilder("float") { self =>
  def left: Widget.Raw.CSSAttr = self := "left"
  def right: Widget.Raw.CSSAttr = self := "right"
  def none: Widget.Raw.CSSAttr = self := "none"
  def inlineStart: Widget.Raw.CSSAttr = self := "inline-start"
  def inlineEnd: Widget.Raw.CSSAttr = self := "inline-end"
}
object clear extends CssBuilder("clear") { self =>
  def left: Widget.Raw.CSSAttr = self := "left"
  def right: Widget.Raw.CSSAttr = self := "right"
  def both: Widget.Raw.CSSAttr = self := "both"
  def none: Widget.Raw.CSSAttr = self := "none"
  def inlineStart: Widget.Raw.CSSAttr = self := "inline-start"
  def inlineEnd: Widget.Raw.CSSAttr = self := "inline-end"
}
object overflow extends CssBuilder("overflow"), CssBuilder.OverflowBuilder
object overflowX extends CssBuilder("overflow-x"), CssBuilder.OverflowBuilder
object overflowY extends CssBuilder("overflow-y"), CssBuilder.OverflowBuilder
object opacity extends CssBuilder("opacity")
object boxShadow extends CssBuilder("box-shadow")
object outline extends CssBuilder("outline")
object outlineColor extends CssBuilder("outline-color")
object outlineStyle extends CssBuilder("outline-style")
object outlineWidth extends CssBuilder("outline-width")
object transition extends CssBuilder("transition")
object transform extends CssBuilder("transform")
object filter extends CssBuilder("filter")
object clipPath extends CssBuilder("clip-path")
object content extends CssBuilder("content")
object verticalAlign extends CssBuilder("vertical-align")
object order extends CssBuilder("order")

// Flexbox
object flex extends CssBuilder("flex")
object flexBasis extends CssBuilder("flex-basis")
object flexDirection extends CssBuilder("flex-direction") { self =>
  def row: Widget.Raw.CSSAttr = self := "row"
  def rowReverse: Widget.Raw.CSSAttr = self := "row-reverse"
  def column: Widget.Raw.CSSAttr = self := "column"
  def columnReverse: Widget.Raw.CSSAttr = self := "column-reverse"
}
object flexFlow extends CssBuilder("flex-flow")
object flexGrow extends CssBuilder("flex-grow")
object flexShrink extends CssBuilder("flex-shrink")
object flexWrap extends CssBuilder("flex-wrap")
object alignItems extends CssBuilder("align-items") { self =>
  def stretch: Widget.Raw.CSSAttr = self := "stretch"
  def center: Widget.Raw.CSSAttr = self := "center"
  def flexStart: Widget.Raw.CSSAttr = self := "flex-start"
  def flexEnd: Widget.Raw.CSSAttr = self := "flex-end"
  def baseline: Widget.Raw.CSSAttr = self := "baseline"
}
object alignContent extends CssBuilder("align-content")
object alignSelf extends CssBuilder("align-self")
object justifyContent extends CssBuilder("justify-content") { self =>
  def flexStart: Widget.Raw.CSSAttr = self := "flex-start"
  def flexEnd: Widget.Raw.CSSAttr = self := "flex-end"
  def center: Widget.Raw.CSSAttr = self := "center"
  def spaceBetween: Widget.Raw.CSSAttr = self := "space-between"
  def spaceAround: Widget.Raw.CSSAttr = self := "space-around"
  def spaceEvenly: Widget.Raw.CSSAttr = self := "space-evenly"
}
object justifyItems extends CssBuilder("justify-items") { self =>
  def stretch: Widget.Raw.CSSAttr = self := "stretch"
  def center: Widget.Raw.CSSAttr = self := "center"
  def start: Widget.Raw.CSSAttr = self := "start"
  def end: Widget.Raw.CSSAttr = self := "end"
  def left: Widget.Raw.CSSAttr = self := "left"
  def right: Widget.Raw.CSSAttr = self := "right"
}
object justifySelf extends CssBuilder("justify-self") { self =>
  def stretch: Widget.Raw.CSSAttr = self := "stretch"
  def center: Widget.Raw.CSSAttr = self := "center"
  def start: Widget.Raw.CSSAttr = self := "start"
  def end: Widget.Raw.CSSAttr = self := "end"
  def left: Widget.Raw.CSSAttr = self := "left"
  def right: Widget.Raw.CSSAttr = self := "right"
}
object gap extends CssBuilder("gap")
object rowGap extends CssBuilder("row-gap")
object columnGap extends CssBuilder("column-gap")

// Grid
object grid extends CssBuilder("grid")
object gridArea extends CssBuilder("grid-area")
object gridTemplate extends CssBuilder("grid-template")
object gridTemplateAreas extends CssBuilder("grid-template-areas")
object gridTemplateColumns extends CssBuilder("grid-template-columns")
object gridTemplateRows extends CssBuilder("grid-template-rows")
object gridColumn extends CssBuilder("grid-column")
object gridRow extends CssBuilder("grid-row")
object gridColumnStart extends CssBuilder("grid-column-start")
object gridColumnEnd extends CssBuilder("grid-column-end")
object gridRowStart extends CssBuilder("grid-row-start")
object gridRowEnd extends CssBuilder("grid-row-end")
object gridAutoColumns extends CssBuilder("grid-auto-columns")
object gridAutoRows extends CssBuilder("grid-auto-rows")
object gridAutoFlow extends CssBuilder("grid-auto-flow")

// Misc
object tabIndex extends CssBuilder("tab-index")
object listStyle extends CssBuilder("list-style")
object listStyleType extends CssBuilder("list-style-type")
object listStylePosition extends CssBuilder("list-style-position")
object listStyleImage extends CssBuilder("list-style-image")
object quotes extends CssBuilder("quotes")
object counterReset extends CssBuilder("counter-reset")
object counterIncrement extends CssBuilder("counter-increment")
object willChange extends CssBuilder("will-change")
object objectPosition extends CssBuilder("object-position")
object wordBreak extends CssBuilder("word-break")
object wordWrap extends CssBuilder("word-wrap")
object hyphens extends CssBuilder("hyphens")
object caretColor extends CssBuilder("caret-color")
object scrollBehavior extends CssBuilder("scroll-behavior")
object scrollSnapType extends CssBuilder("scroll-snap-type")
object scrollSnapAlign extends CssBuilder("scroll-snap-align")
object scrollMargin extends CssBuilder("scroll-margin")
object scrollPadding extends CssBuilder("scroll-padding")
object touchAction extends CssBuilder("touch-action")
object accentColor extends CssBuilder("accent-color")
object aspectRatio extends CssBuilder("aspect-ratio")
object inset extends CssBuilder("inset")
object insetBlock extends CssBuilder("inset-block")
object insetInline extends CssBuilder("inset-inline")
object insetBlockStart extends CssBuilder("inset-block-start")
object insetBlockEnd extends CssBuilder("inset-block-end")
object insetInlineStart extends CssBuilder("inset-inline-start")
object insetInlineEnd extends CssBuilder("inset-inline-end")

object boxSizing extends CssBuilder("box-sizing") { self =>
  def contentBox: Widget.Raw.CSSAttr = self := "content-box"
  def borderBox: Widget.Raw.CSSAttr = self := "border-box"
}
object pointerEvents extends CssBuilder("pointer-events") { self =>
  def auto: Widget.Raw.CSSAttr = self := "auto"
  def none: Widget.Raw.CSSAttr = self := "none"
  def visible: Widget.Raw.CSSAttr = self := "visible"
  def visibleFill: Widget.Raw.CSSAttr = self := "visibleFill"
  def visibleStroke: Widget.Raw.CSSAttr = self := "visibleStroke"
  def visiblePainted: Widget.Raw.CSSAttr = self := "visiblePainted"
  def painted: Widget.Raw.CSSAttr = self := "painted"
  def fill: Widget.Raw.CSSAttr = self := "fill"
  def stroke: Widget.Raw.CSSAttr = self := "stroke"
  def all: Widget.Raw.CSSAttr = self := "all"
}
object resize extends CssBuilder("resize") { self =>
  def none: Widget.Raw.CSSAttr = self := "none"
  def both: Widget.Raw.CSSAttr = self := "both"
  def horizontal: Widget.Raw.CSSAttr = self := "horizontal"
  def vertical: Widget.Raw.CSSAttr = self := "vertical"
  def block: Widget.Raw.CSSAttr = self := "block"
  def inline: Widget.Raw.CSSAttr = self := "inline"
}
object objectFit extends CssBuilder("object-fit") { self =>
  def fill: Widget.Raw.CSSAttr = self := "fill"
  def contain: Widget.Raw.CSSAttr = self := "contain"
  def cover: Widget.Raw.CSSAttr = self := "cover"
  def none: Widget.Raw.CSSAttr = self := "none"
  def scaleDown: Widget.Raw.CSSAttr = self := "scale-down"
}
object isolation extends CssBuilder("isolation") { self =>
  def auto: Widget.Raw.CSSAttr = self := "auto"
  def isolate: Widget.Raw.CSSAttr = self := "isolate"
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

abstract class CssBuilder(key: String) { self =>

  final def :=(value: String): Widget.Raw.CSSAttr = Widget.Raw.css(key, value)

  final def inherit: Widget.Raw.CSSAttr = self := "inherit"
  final def initial: Widget.Raw.CSSAttr = self := "initial"
  final def revert: Widget.Raw.CSSAttr = self := "revert"
  final def revertLayer: Widget.Raw.CSSAttr = self := "revert-layer"
  final def unset: Widget.Raw.CSSAttr = self := "unset"

}
object CssBuilder {

  trait ColorBuilder { self: CssBuilder =>
    final def red: Widget.Raw.CSSAttr = self := "red"
    final def blue: Widget.Raw.CSSAttr = self := "blue"
    final def green: Widget.Raw.CSSAttr = self := "green"
    final def black: Widget.Raw.CSSAttr = self := "black"
    final def white: Widget.Raw.CSSAttr = self := "white"
    final def gray: Widget.Raw.CSSAttr = self := "gray"
    final def yellow: Widget.Raw.CSSAttr = self := "yellow"
    final def orange: Widget.Raw.CSSAttr = self := "orange"
    final def purple: Widget.Raw.CSSAttr = self := "purple"
    final def brown: Widget.Raw.CSSAttr = self := "brown"
    final def pink: Widget.Raw.CSSAttr = self := "pink"
    final def cyan: Widget.Raw.CSSAttr = self := "cyan"
    final def magenta: Widget.Raw.CSSAttr = self := "magenta"
    final def transparent: Widget.Raw.CSSAttr = self := "transparent"
    final def silver: Widget.Raw.CSSAttr = self := "silver"
    final def lime: Widget.Raw.CSSAttr = self := "lime"
    final def maroon: Widget.Raw.CSSAttr = self := "maroon"
    final def olive: Widget.Raw.CSSAttr = self := "olive"
    final def teal: Widget.Raw.CSSAttr = self := "teal"
    final def navy: Widget.Raw.CSSAttr = self := "navy"
    final def fuchsia: Widget.Raw.CSSAttr = self := "fuchsia"
    final def aqua: Widget.Raw.CSSAttr = self := "aqua"
    final def rgb(r: Int, g: Int, b: Int): Widget.Raw.CSSAttr = self := s"rgb($r, $g, $b)"
  }

  trait SpacingBuilder { self: CssBuilder =>
    final def apply(allSides: String): Widget.Raw.CSSAttr = self := allSides
    final def apply(topBottom: String, leftRight: String): Widget.Raw.CSSAttr = self := s"$topBottom $leftRight"
    final def apply(top: String, leftRight: String, bottom: String): Widget.Raw.CSSAttr = self := s"$top $leftRight $bottom"
    final def apply(top: String, right: String, bottom: String, left: String): Widget.Raw.CSSAttr = self := s"$top $right $bottom $left"
  }

  trait OverflowBuilder { self: CssBuilder =>
    def visible: Widget.Raw.CSSAttr = self := "visible"
    def hidden: Widget.Raw.CSSAttr = self := "hidden"
    def scroll: Widget.Raw.CSSAttr = self := "scroll"
    def auto: Widget.Raw.CSSAttr = self := "auto"
    def clip: Widget.Raw.CSSAttr = self := "clip"
  }
}
