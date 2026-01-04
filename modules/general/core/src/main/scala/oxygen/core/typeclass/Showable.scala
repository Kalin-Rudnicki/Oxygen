package oxygen.core.typeclass

import oxygen.core.{ColorStateV2, LazyString, StringBuilder}

trait Showable extends LazyString.UnsafeCustom {

  def show: LazyString

  override def lazyStringWriteSimple(cfg: LazyString.Config, builder: StringBuilder): Unit =
    show.lazyStringWriteSimple(cfg, builder)

  override def lazyStringWriteComplex(cfg: LazyString.Config, builder: StringBuilder, currentIndent: String, colorState: ColorStateV2): Unit =
    show.lazyStringWriteComplex(cfg, builder, currentIndent, colorState)

}
