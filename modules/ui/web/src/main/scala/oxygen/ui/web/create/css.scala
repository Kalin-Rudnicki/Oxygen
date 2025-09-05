package oxygen.ui.web.create

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Special
//////////////////////////////////////////////////////////////////////////////////////////////////////

object cursor extends CssBuilder("cursor") { self =>
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

object textAlign extends CssBuilder("text-align") { self =>
  def start: CSSAttr = self := "start"
  def end: CSSAttr = self := "end"
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
  def center: CSSAttr = self := "center"
  def justify: CSSAttr = self := "justify"
  def matchParent: CSSAttr = self := "match-parent"
}

object verticalAlign extends CssBuilder("vertical-align") { self =>
  def baseline: CSSAttr = self := "baseline"
  def sub: CSSAttr = self := "sub"
  def `super`: CSSAttr = self := "super"
  def textTop: CSSAttr = self := "text-top"
  def textBottom: CSSAttr = self := "text-bottom"
  def middle: CSSAttr = self := "middle"
  def top: CSSAttr = self := "top"
  def bottom: CSSAttr = self := "bottom"
}

object display extends CssBuilder("display") { self =>

  def block: CSSAttr = self := "block"
  def inline: CSSAttr = self := "inline"
  def inlineBlock: CSSAttr = self := "inline-block"
  def flex: CSSAttr = self := "flex"
  def inlineFlex: CSSAttr = self := "inline-flex"
  def grid: CSSAttr = self := "grid"
  def inlineGrid: CSSAttr = self := "inline-grid"
  def flowRoot: CSSAttr = self := "flow-root"

}

object whiteSpace extends CssBuilder("white-space") { self =>
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

object position extends CssBuilder("position") { self =>
  def static: CSSAttr = self := "static"
  def relative: CSSAttr = self := "relative"
  def absolute: CSSAttr = self := "absolute"
  def fixed: CSSAttr = self := "fixed"
  def sticky: CSSAttr = self := "sticky"
}

object visibility extends CssBuilder("visibility") { self =>
  def visible: CSSAttr = self := "visible"
  def hidden: CSSAttr = self := "hidden"
  def collapse: CSSAttr = self := "collapse"
}

object userSelect extends CssBuilder("user-select") { self =>
  def auto: CSSAttr = self := "auto"
  def text: CSSAttr = self := "text"
  def none: CSSAttr = self := "none"
  def contain: CSSAttr = self := "contain"
  def all: CSSAttr = self := "all"
}

object borderRadius extends CssBuilder("border-radius")
object borderBottomLeftRadius extends CssBuilder("border-bottom-left-radius")
object borderBottomRightRadius extends CssBuilder("border-bottom-right-radius")
object borderTopLeftRadius extends CssBuilder("border-top-left-radius")
object borderTopRightRadius extends CssBuilder("border-top-right-radius")

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Other
//////////////////////////////////////////////////////////////////////////////////////////////////////

object width extends CssBuilder("width") { self =>
  def fitContent: CSSAttr = self := "fit-content"
}

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
object fontFamily extends CssBuilder("font-family")
object fontStyle extends CssBuilder("font-style") { self =>
  def normal: CSSAttr = self := "normal"
  def italic: CSSAttr = self := "italic"
  def oblique: CSSAttr = self := "oblique"
}
object fontVariant extends CssBuilder("font-variant")
object fontStretch extends CssBuilder("font-stretch")
object textDecoration extends CssBuilder("text-decoration")
object textTransform extends CssBuilder("text-transform") { self =>
  def none: CSSAttr = self := "none"
  def capitalize: CSSAttr = self := "capitalize"
  def uppercase: CSSAttr = self := "uppercase"
  def lowercase: CSSAttr = self := "lowercase"
  def fullWidth: CSSAttr = self := "full-width"
}
object textOverflow extends CssBuilder("text-overflow")
object lineHeight extends CssBuilder("line-height")
object letterSpacing extends CssBuilder("letter-spacing")
object wordSpacing extends CssBuilder("word-spacing")
object direction extends CssBuilder("direction")
object unicodeBidi extends CssBuilder("unicode-bidi")

object border extends CssBuilder("border"), CssBuilder.BorderBuilder
object borderTop extends CssBuilder("border-top"), CssBuilder.BorderBuilder
object borderRight extends CssBuilder("border-right"), CssBuilder.BorderBuilder
object borderBottom extends CssBuilder("border-bottom"), CssBuilder.BorderBuilder
object borderLeft extends CssBuilder("border-left"), CssBuilder.BorderBuilder

object borderColor extends CssBuilder("border-color"), CssBuilder.ColorBuilder
object borderStyle extends CssBuilder("border-style") { self =>
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
object borderWidth extends CssBuilder("border-width")
object borderCollapse extends CssBuilder("border-collapse") { self =>
  def separate: CSSAttr = self := "separate"
  def collapse: CSSAttr = self := "collapse"
}
object borderSpacing extends CssBuilder("border-spacing")

object top extends CssBuilder("top")
object right extends CssBuilder("right")
object bottom extends CssBuilder("bottom")
object left extends CssBuilder("left")
object zIndex extends CssBuilder("z-index")
object float extends CssBuilder("float") { self =>
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
  def none: CSSAttr = self := "none"
  def inlineStart: CSSAttr = self := "inline-start"
  def inlineEnd: CSSAttr = self := "inline-end"
}
object clear extends CssBuilder("clear") { self =>
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
  def both: CSSAttr = self := "both"
  def none: CSSAttr = self := "none"
  def inlineStart: CSSAttr = self := "inline-start"
  def inlineEnd: CSSAttr = self := "inline-end"
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
object order extends CssBuilder("order")

// Flexbox
object flex extends CssBuilder("flex")
object flexBasis extends CssBuilder("flex-basis")
object flexDirection extends CssBuilder("flex-direction") { self =>
  def row: CSSAttr = self := "row"
  def rowReverse: CSSAttr = self := "row-reverse"
  def column: CSSAttr = self := "column"
  def columnReverse: CSSAttr = self := "column-reverse"
}
object flexFlow extends CssBuilder("flex-flow")
object flexGrow extends CssBuilder("flex-grow")
object flexShrink extends CssBuilder("flex-shrink")
object flexWrap extends CssBuilder("flex-wrap") { self =>
  def nowrap: CSSAttr = self := "nowrap"
  def wrap: CSSAttr = self := "wrap"
  def wrapReverse: CSSAttr = self := "wrap-reverse"
}
object alignItems extends CssBuilder("align-items") { self =>
  def stretch: CSSAttr = self := "stretch"
  def center: CSSAttr = self := "center"
  def flexStart: CSSAttr = self := "flex-start"
  def flexEnd: CSSAttr = self := "flex-end"
  def baseline: CSSAttr = self := "baseline"
}
object alignContent extends CssBuilder("align-content")
object alignSelf extends CssBuilder("align-self")
object justifyContent extends CssBuilder("justify-content") { self =>
  def flexStart: CSSAttr = self := "flex-start"
  def flexEnd: CSSAttr = self := "flex-end"
  def center: CSSAttr = self := "center"
  def spaceBetween: CSSAttr = self := "space-between"
  def spaceAround: CSSAttr = self := "space-around"
  def spaceEvenly: CSSAttr = self := "space-evenly"
}
object justifyItems extends CssBuilder("justify-items") { self =>
  def stretch: CSSAttr = self := "stretch"
  def center: CSSAttr = self := "center"
  def start: CSSAttr = self := "start"
  def end: CSSAttr = self := "end"
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
}
object justifySelf extends CssBuilder("justify-self") { self =>
  def stretch: CSSAttr = self := "stretch"
  def center: CSSAttr = self := "center"
  def start: CSSAttr = self := "start"
  def end: CSSAttr = self := "end"
  def left: CSSAttr = self := "left"
  def right: CSSAttr = self := "right"
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
  def contentBox: CSSAttr = self := "content-box"
  def borderBox: CSSAttr = self := "border-box"
}
object pointerEvents extends CssBuilder("pointer-events") { self =>
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
object resize extends CssBuilder("resize") { self =>
  def none: CSSAttr = self := "none"
  def both: CSSAttr = self := "both"
  def horizontal: CSSAttr = self := "horizontal"
  def vertical: CSSAttr = self := "vertical"
  def block: CSSAttr = self := "block"
  def inline: CSSAttr = self := "inline"
}
object objectFit extends CssBuilder("object-fit") { self =>
  def fill: CSSAttr = self := "fill"
  def contain: CSSAttr = self := "contain"
  def cover: CSSAttr = self := "cover"
  def none: CSSAttr = self := "none"
  def scaleDown: CSSAttr = self := "scale-down"
}
object isolation extends CssBuilder("isolation") { self =>
  def auto: CSSAttr = self := "auto"
  def isolate: CSSAttr = self := "isolate"
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

abstract class CssBuilder(key: String) { self =>

  final def :=(value: String): CSSAttr = Widget.raw.css(key, value)
  final def :=(value: Int): CSSAttr = self := value.toString

  def csss(strs: String*): CSSAttr = self := strs.mkString(" ")

  final def inherit: CSSAttr = self := "inherit"
  final def initial: CSSAttr = self := "initial"
  final def revert: CSSAttr = self := "revert"
  final def revertLayer: CSSAttr = self := "revert-layer"
  final def unset: CSSAttr = self := "unset"

}
object CssBuilder {

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

  }

}
