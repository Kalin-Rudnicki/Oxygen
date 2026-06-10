package oxygen.cli

import oxygen.schema.PlainTextSchema
import zio.*

/** A path string whose tab completion is scoped to a runtime-provided base directory. See [[GlobalPath]] for a compile-time base. */
opaque type RelativePath <: String = String

object RelativePath {

  def apply(value: String): RelativePath = value

  extension (path: RelativePath) def value: String = path

  given PlainTextSchema[RelativePath] = PlainTextSchema.string.transform(apply, _.value)

  def under(basePath: String): CompletionOptions[RelativePath] =
    new CompletionOptions[RelativePath] {
      override def completionOptions(in: String): Task[Seq[String]] =
        CompletionOptions.underBasePath(basePath).completionOptions(in).map(_.map(apply))
    }

}
