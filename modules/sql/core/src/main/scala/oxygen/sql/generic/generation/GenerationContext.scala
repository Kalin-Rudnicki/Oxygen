package oxygen.sql.generic.generation

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.TypeclassExpr
import oxygen.sql.generic.parsing.*
import scala.quoted.*
import scala.util.NotGiven

final case class GenerationContext private (
    handleOptionalInputs: GenerationContext.HandleOptionalInputs,
    query: GenerationContext.Parens,
    input: GenerationContext.Parens,
)
object GenerationContext {

  private val default = GenerationContext(HandleOptionalInputs.Error, Parens.IfMulti, Parens.IfMulti)

  sealed trait HandleOptionalInputs
  object HandleOptionalInputs {

    case object Error extends HandleOptionalInputs

    final case class Allowed(
        make: (TypeRepr, GeneratedFragment) => GeneratedFragment,
    ) extends HandleOptionalInputs

    val id: HandleOptionalInputs =
      Allowed { (_, frag) => frag }

    def or(f: GeneratedFragment => GeneratedFragment): HandleOptionalInputs =
      Allowed { (tpe, frag) =>
        GeneratedFragment.of(
          GeneratedFragment.both(
            GeneratedSql.single("( ? OR ( "),
            GeneratedInputEncoder.nonConst(TypeclassExpr.InputEncoder.isNullEncoder, tpe),
          ),
          f(frag),
          " ) )",
        )
      }

  }

  enum Parens {
    case Always, IfMulti, Never

    final def `ref.a, ref.b, ref.c`(cols: TypeclassExpr.Columns, ref: Expr[String]): GeneratedSql = cols.`ref.a, ref.b, ref.c`(this, ref)
    final def `a, b, c`(cols: TypeclassExpr.Columns): GeneratedSql = cols.`a, b, c`(this)
    final def `?, ?, ?`(cols: TypeclassExpr.Columns): GeneratedSql = cols.`?, ?, ?`(this)

  }

  def optionalInputHandling(term: Term)(using gc: GenerationContext, pc: ParseContext): ParseResult[HandleOptionalInputs.Allowed] =
    gc.handleOptionalInputs match
      case allowed: HandleOptionalInputs.Allowed => ParseResult.success(allowed)
      case HandleOptionalInputs.Error            => ParseResult.error(term, "optional input not allowed here")

  def get(using ctx: GenerationContext): GenerationContext = ctx

  def root[A](f: GenerationContext ?=> A)(using NotGiven[GenerationContext]): A =
    f(using default)

  def updated[A](
      handleOptionalInputs: Specified[HandleOptionalInputs] = Specified.WasNotSpecified,
      query: Specified[GenerationContext.Parens] = Specified.WasNotSpecified,
      input: Specified[GenerationContext.Parens] = Specified.WasNotSpecified,
  )(f: GenerationContext ?=> A)(using ctx0: GenerationContext): A = {
    val ctx1: GenerationContext =
      GenerationContext(
        handleOptionalInputs = handleOptionalInputs.getOrElse(ctx0.handleOptionalInputs),
        query = query.getOrElse(ctx0.query),
        input = input.getOrElse(ctx0.input),
      )
    f(using ctx1)
  }

}
