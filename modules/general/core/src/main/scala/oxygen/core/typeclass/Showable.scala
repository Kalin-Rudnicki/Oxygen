package oxygen.core.typeclass

import oxygen.core.{ColorStateV2, StringBuilder, Text}

trait Showable extends Text.UnsafeCustom {

  def show: Text

  override def textWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit =
    show.textWriteSimple(cfg, builder)

  override def textWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
    show.textWriteComplex(cfg, builder, currentIndent, colorState)

}
