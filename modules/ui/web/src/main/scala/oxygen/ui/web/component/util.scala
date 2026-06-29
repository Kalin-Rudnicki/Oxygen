package oxygen.ui.web.component

object util {

  val nonBreakingSpace: String = "\u00A0"

  def staticTemplateAreas(areaGroups: Seq[String]*): String =
    areaGroups.map { _.mkString("\n    '", " ", "'") }.mkString

  def dynamicTemplateAreas(areaGroups: (Boolean, Seq[(Boolean, String)])*): String =
    areaGroups
      .collect { case (true, gs) =>
        gs.collect { case (true, g) => g }.mkString("\n    '", " ", "'")
      }.mkString

  def dynamicTemplateSizes(sizes: (Boolean, String)*): String =
    sizes.collect { case (true, s) => s }.mkString(" ")

}
