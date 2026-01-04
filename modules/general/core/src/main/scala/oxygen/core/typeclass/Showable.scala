package oxygen.core.typeclass

import oxygen.core.{LazyString, StringBuilder}

trait Showable extends LazyString.UnsafeCustom {

  def show: LazyString

  override def showableStringBuilderWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
    show.showableStringBuilderWriteSimple(cfg, builder)

  override def showableStringBuilderWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: LazyString.ColorState): Unit =
    show.showableStringBuilderWriteComplex(cfg, builder, currentIndent, colorState)

  override final def toString: String = show.toString

}
