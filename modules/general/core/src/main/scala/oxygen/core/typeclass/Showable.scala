package oxygen.core.typeclass

import oxygen.core.{ColorStateV2, LazyString, StringBuilder}

trait Showable extends LazyString.UnsafeCustom {

  def show: LazyString

  override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
    show.showableStringBuilderWriteSimple(cfg, builder)

  override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
    show.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)

}
