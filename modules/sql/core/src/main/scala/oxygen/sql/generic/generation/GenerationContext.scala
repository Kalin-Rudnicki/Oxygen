package oxygen.sql.generic.generation

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.TypeclassExpr
import oxygen.sql.generic.parsing.*
import scala.quoted.*
import scala.util.NotGiven

final case class GenerationContext private (
    allowOptionalInput: Boolean,
    query: GenerationContext.Parens,
    input: GenerationContext.Parens,
)
object GenerationContext {

  private val default = GenerationContext(false, Parens.IfMulti, Parens.IfMulti)

  enum Parens {
    case Always, IfMulti, Never

    final def `ref.a, ref.b, ref.c`(cols: TypeclassExpr.Columns, ref: Expr[String])(using Quotes): GeneratedSql = cols.`ref.a, ref.b, ref.c`(this, ref)
    final def `a, b, c`(cols: TypeclassExpr.Columns)(using Quotes): GeneratedSql = cols.`a, b, c`(this)
    final def `?, ?, ?`(cols: TypeclassExpr.Columns)(using Quotes): GeneratedSql = cols.`?, ?, ?`(this)

  }

  def enforceOptionalInputAllowed(term: Term)(using gc: GenerationContext, pc: ParseContext): ParseResult[Unit] =
    if (gc.allowOptionalInput) ParseResult.Success(())
    else ParseResult.error(term, "optional input not allowed here")

  def get(using ctx: GenerationContext): GenerationContext = ctx

  def root[A](f: GenerationContext ?=> A)(using NotGiven[GenerationContext]): A =
    f(using default)

  def updated[A](
      allowOptionalInput: Specified[Boolean] = Specified.WasNotSpecified,
      query: Specified[GenerationContext.Parens] = Specified.WasNotSpecified,
      input: Specified[GenerationContext.Parens] = Specified.WasNotSpecified,
  )(f: GenerationContext ?=> A)(using ctx: GenerationContext): A =
    f(using GenerationContext(allowOptionalInput = allowOptionalInput.getOrElse(ctx.allowOptionalInput), query = query.getOrElse(ctx.query), input = input.getOrElse(ctx.input)))

}
