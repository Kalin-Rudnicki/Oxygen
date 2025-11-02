package oxygen.ui.web.create

import oxygen.predef.core.*

object StandardProps {

  final case class Padding(topBottom: String, leftRight: String) derives Show {
    val show: String = s"$topBottom $leftRight"
  }
  object Padding {
    val none: Padding = Padding("0px", "0px")
  }

  final case class DerivedColors(
      _baseColor: CSSColor,
      _color: Specified[DerivedColors.Group] = ___,
      _backgroundColor: Specified[DerivedColors.Group] = ___,
      _borderColor: Specified[DerivedColors.Group] = ___,
  ) derives Show {

    def baseColor(color: CSSColor): DerivedColors = copy(_baseColor = color)
    def baseColor(color: CSSVar): DerivedColors = copy(_baseColor = color.getColorValue)
    def baseColor(color: String): DerivedColors = baseColor(CSSColor.eval(color))

    def mod(
        modColor: DerivedColors.Group => DerivedColors.Group = identity,
        modBackgroundColor: DerivedColors.Group => DerivedColors.Group = identity,
        modBorderColor: DerivedColors.Group => DerivedColors.Group = identity,
    ): DerivedColors =
      copy(
        _color = modColor(_color.getOrElse(DerivedColors.Group())).specifiedIfNonEmpty,
        _backgroundColor = modBackgroundColor(_backgroundColor.getOrElse(DerivedColors.Group())).specifiedIfNonEmpty,
        _borderColor = modBorderColor(_borderColor.getOrElse(DerivedColors.Group())).specifiedIfNonEmpty,
      )

    def setColor(color: ColorTransform): DerivedColors = copy(_color = DerivedColors.Group(_base = color))
    def setColor(color: CSSColor): DerivedColors = setColor(ColorTransform.const(color))
    def setColor(color: CSSVar): DerivedColors = setColor(ColorTransform.const(color))
    def setColor(color: String): DerivedColors = setColor(ColorTransform.const(color))

    def setBackgroundColor(base: ColorTransform): DerivedColors = copy(_backgroundColor = DerivedColors.Group(_base = base))
    def setBackgroundColor(color: CSSColor): DerivedColors = setBackgroundColor(ColorTransform.const(color))
    def setBackgroundColor(color: CSSVar): DerivedColors = setBackgroundColor(ColorTransform.const(color))
    def setBackgroundColor(color: String): DerivedColors = setBackgroundColor(ColorTransform.const(color))

    def setBorderColor(base: ColorTransform): DerivedColors = copy(_borderColor = DerivedColors.Group(_base = base))
    def setBorderColor(color: CSSColor): DerivedColors = setBorderColor(ColorTransform.const(color))
    def setBorderColor(color: CSSVar): DerivedColors = setBorderColor(ColorTransform.const(color))
    def setBorderColor(color: String): DerivedColors = setBorderColor(ColorTransform.const(color))

    extension (self: Specified[DerivedColors.Group])
      private def eval(builder: CssBuilder): Widget = self match
        case Specified.WasSpecified(group) => group.apply(builder, _baseColor)
        case Specified.WasNotSpecified     => Widget.empty

    lazy val styles: Widget =
      fragment(
        _color.eval(color),
        _backgroundColor.eval(backgroundColor),
        _borderColor.eval(borderColor),
      )

  }
  object DerivedColors {

    final case class Group(
        _base: Specified[ColorTransform] = ___,
        _hover: Specified[ColorTransform] = ___,
        _active: Specified[ColorTransform] = ___,
        _hoverActive: Specified[ColorTransform] = ___,
        _focus: Specified[ColorTransform] = ___,
        _hoverFocus: Specified[ColorTransform] = ___,
    ) derives Show {

      lazy val nonEmpty: Boolean =
        Seq(
          _base,
          _hover,
          _active,
          _hoverActive,
          _focus,
          _hoverFocus,
        ).exists(_.isSpecified)

      def specifiedIfNonEmpty: Specified[Group] = if (this.nonEmpty) this else ___

      extension (self: Specified[ColorTransform])
        private def eval(builder: CssBuilder#InlinePseudoAttr, color: CSSColor): Widget = self match
          case Specified.WasSpecified(transform) => builder := transform.transform(color)
          case Specified.WasNotSpecified         => Widget.empty

      def apply(builder: CssBuilder, color: CSSColor): Widget =
        fragment(
          _base.eval(builder.dynamic, color),
          _hover.eval(builder.dynamic.hover, color),
          _active.eval(builder.dynamic.active, color),
          _hoverActive.eval(builder.dynamic.hoverActive, color),
          _focus.eval(builder.dynamic.focus, color),
          _hoverFocus.eval(builder.dynamic.hoverFocus, color),
        )

    }

    lazy val empty: DerivedColors =
      DerivedColors(S.color.primary.getColorValue)

  }

}
