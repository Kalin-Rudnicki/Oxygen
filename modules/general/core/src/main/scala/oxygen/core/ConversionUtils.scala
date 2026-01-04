package oxygen.core

object ConversionUtils {

  private object ConvertId extends Conversion[Any, Any] {
    override def apply(x: Any): Any = x
  }

  def id[A]: Conversion[A, A] = ConvertId.asInstanceOf[Conversion[A, A]]

}
