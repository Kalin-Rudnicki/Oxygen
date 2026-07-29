package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * HolyGrail-style label (W2-T05).
  */
final case class Label(
    private val labelText: String,
    private val description: Option[Widget],
    private val labelMarginLeft: String,
    private val descriptionMarginLeft: String,
    private val labelDescriptionSpacing: String,
    private val labelExtra: Widget,
    private val descriptionExtra: Widget,
    private val rootExtra: Widget,
) extends PWidget.Deferred[Any, Nothing, Any, Nothing] {

  def text(t: String): Label = copy(labelText = t)
  def describe(d: Widget): Label = copy(description = d.some)
  def labelMarginLeft(v: String): Label = copy(labelMarginLeft = v)
  def descriptionMarginLeft(v: String): Label = copy(descriptionMarginLeft = v)

  def labelExtra(mods: Widget*): Label = copy(labelExtra = fragment(this.labelExtra, Widget.fragment(mods)))
  def descriptionExtra(mods: Widget*): Label = copy(descriptionExtra = fragment(this.descriptionExtra, Widget.fragment(mods)))
  def rootExtra(mods: Widget*): Label = copy(rootExtra = fragment(this.rootExtra, Widget.fragment(mods)))

  /** Alias for label text extras (former Decorator.mod / labelMod). */
  def mod(mods: Widget*): Label = labelExtra(mods*)

  override protected def build: PWidget[Any, Nothing, Any, Nothing] =
    div(
      O.Label,
      div(
        O.Label.LabelText,
        marginLeft := labelMarginLeft,
        fontWeight := S.fontWeight.semiBold,
        fontSize := S.fontSize._4,
        labelText,
        labelExtra,
      ),
      Widget.foreach(description) { desc =>
        div(
          O.Label.DescriptionText,
          whiteSpace.pre,
          marginTop := labelDescriptionSpacing,
          marginLeft := descriptionMarginLeft,
          desc,
          descriptionExtra,
        )
      },
      rootExtra,
    )

}
object Label {

  val empty: Label =
    Label(
      labelText = "",
      description = None,
      labelMarginLeft = S.spacing._6,
      descriptionMarginLeft = S.spacing._3,
      labelDescriptionSpacing = S.spacing._1,
      labelExtra = Widget.empty,
      descriptionExtra = Widget.empty,
      rootExtra = Widget.empty,
    )

  def apply(text: String): Label = empty.text(text)

  def apply(): Label = empty

  lazy val defaultInputSpacing: String = S.spacing._2

}
