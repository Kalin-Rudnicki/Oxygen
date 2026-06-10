package oxygen.cli

import oxygen.schema.PlainTextSchema
import scala.compiletime.constValue
import zio.*

/**
  * A path string whose tab-completion root is fixed at compile time via a string-literal type parameter.
  *
  * {{{
  * @named config: GlobalPath["./configs"]
  * }}}
  */
opaque type GlobalPath[Base <: String & Singleton] <: String = String

object GlobalPath {

  def apply[Base <: String & Singleton](value: String): GlobalPath[Base] = value

  extension [Base <: String & Singleton](path: GlobalPath[Base]) def value: String = path

  inline def base[Base <: String & Singleton]: String = constValue[Base]

  inline given [Base <: String & Singleton] => PlainTextSchema[GlobalPath[Base]] =
    PlainTextSchema.string.transform(apply[Base], identity)

  final case class Completion[Base <: String & Singleton](base: String) extends CompletionOptions[GlobalPath[Base]] {
    override def completionOptions(in: String): Task[Seq[String]] =
      CompletionOptions.underBasePath(base).completionOptions(in).map(_.map(apply[Base]))
  }

  inline given [Base <: String & Singleton] => CompletionOptions[GlobalPath[Base]] =
    Completion[Base](constValue[Base])

}
