package oxygen.core.typeclass

import oxygen.core.{ColorStateV2, StringBuilder, Text}

trait Showable extends Text.UnsafeCustom {

  def show: Text

  override def lazyStringWriteSimple(cfg: Text.Config, builder: StringBuilder): Unit =
    show.lazyStringWriteSimple(cfg, builder)

  override def lazyStringWriteComplex(cfg: Text.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
    show.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)

}
