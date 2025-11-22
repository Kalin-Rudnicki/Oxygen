package oxygen.core.codec

import oxygen.core.TypeTag

trait InfallibleTransform[In, Out, +InFormat <: Format, +OutFormat <: Format] {
  
  val inFormat: InFormat
  val outFormat: OutFormat
  
  protected def transformImpl(in: In): Out
  
  def apply(in: In): Out
  
}
