package oxygen.ui.web.create

import oxygen.ui.web.internal.{StyleSheetElement, StyleSheetSelector}
import zio.internal.stacktracer.SourceLocation

given textToWidget: Conversion[String, Widget] = Widget.text(_)

given cssAttrToElement: (loc: SourceLocation) => Conversion[CSSAttr, StyleSheetElement] =
  attr => StyleSheetElement.CSS(attr.raw.key, attr.raw.value, loc)

given hasSelectorToSelector: Conversion[StyleSheetBuilder.HasSelector, StyleSheetSelector.RootWithoutTag] =
  _.selector

given hasSelectorToWidget: Conversion[StyleSheetBuilder.HasSelector, ClassAttr] =
  hs => Widget.raw.`class`(hs.classNames)

given cssVarToString: Conversion[CSSVar, String] =
  _.varString

given oxygenStyleVarsStrengthVar: Conversion[OxygenStyleVars.ColorWithStrength, CSSVar] = _.standard
given oxygenStyleVarsLightDarkVar: Conversion[OxygenStyleVars.ColorWithLightDark, CSSVar] = _.standard
given oxygenStyleVarsStrengthString: Conversion[OxygenStyleVars.ColorWithStrength, String] = _.standard
given oxygenStyleVarsLightDarkString: Conversion[OxygenStyleVars.ColorWithLightDark, String] = _.standard

given cssColorToString: Conversion[CSSColor, String] = _.toString
