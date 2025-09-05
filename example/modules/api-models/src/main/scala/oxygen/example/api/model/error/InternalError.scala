package oxygen.example.api.model.error

import oxygen.schema.JsonSchema

final case class InternalError(
    error: String,
    trace: Seq[String],
) derives JsonSchema {

  def show: String = s"InternalError: $error${trace.map { t => s"\n  - $t" }.mkString}"
  override def toString: String = show

}
