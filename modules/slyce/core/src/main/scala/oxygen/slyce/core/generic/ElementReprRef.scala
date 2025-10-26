package oxygen.slyce.core.generic

import oxygen.quoted.*

private[generic] final class ElementReprRef[A <: ElementRepr](typeRepr: TypeRepr) {
  private var isInitialized: Boolean = false
  private var valueRef: A = null.asInstanceOf[A]

  def value: A = {
    if (!isInitialized) throw new RuntimeException(s"attempted to get uninitialized value for: ${typeRepr.showAnsiCode}")
    valueRef
  }

  def init(value: A): Unit = {
    if (isInitialized) throw new RuntimeException(s"attempted to init already initialized value for: ${typeRepr.showAnsiCode}")
    isInitialized = true
    valueRef = value
  }

  override def toString: String =
    if (isInitialized) valueRef.toString
    else s"< uninitialized : ${typeRepr.showAnsiCode} >"

}
