package oxygen.sql.generic.generation

import oxygen.predef.core.*
import oxygen.sql.generic.model.TypeclassExpr
import scala.quoted.*
import scala.util.NotGiven

final case class GenerationContext private (
    query: GenerationContext.Parens,
    input: GenerationContext.Parens,
)
object GenerationContext {

  private val default = GenerationContext(Parens.IfMulti, Parens.IfMulti)

  enum Parens {
    case Always, IfMulti, Never

    final def `ref.a, ref.b, ref.c`(cols: TypeclassExpr.Columns, ref: Expr[String])(using Quotes): GeneratedSql = cols.`ref.a, ref.b, ref.c`(this, ref)
    final def `a, b, c`(cols: TypeclassExpr.Columns)(using Quotes): GeneratedSql = cols.`a, b, c`(this)
    final def `?, ?, ?`(cols: TypeclassExpr.Columns)(using Quotes): GeneratedSql = cols.`?, ?, ?`(this)

  }

  def get(using ctx: GenerationContext): GenerationContext = ctx

  def root[A](f: GenerationContext ?=> A)(using NotGiven[GenerationContext]): A =
    f(using default)

  def updated[A](
      query: Specified[GenerationContext.Parens] = Specified.WasNotSpecified,
      input: Specified[GenerationContext.Parens] = Specified.WasNotSpecified,
  )(f: GenerationContext ?=> A)(using ctx: GenerationContext): A =
    f(using GenerationContext(query = query.getOrElse(ctx.query), input = input.getOrElse(ctx.input)))

}
