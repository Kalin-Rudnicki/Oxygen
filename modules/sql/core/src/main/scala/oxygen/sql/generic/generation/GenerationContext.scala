package oxygen.sql.generic.generation

import oxygen.predef.core.*
import oxygen.sql.schema.*
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

    final def `ref.a, ref.b, ref.c`(expr: Expr[Columns[?]], ref: Expr[String])(using Quotes): GeneratedSql = this match
      case Parens.Always  => GeneratedSql.of("(", '{ $expr.`ref.a, ref.b, ref.c`($ref) }, ")")
      case Parens.IfMulti => GeneratedSql.of('{ $expr.`(ref.a, ref.b, ref.c)`($ref) })
      case Parens.Never   => GeneratedSql.of('{ $expr.`ref.a, ref.b, ref.c`($ref) })

    final def `a, b, c`(expr: Expr[Columns[?]])(using Quotes): GeneratedSql = this match
      case Parens.Always  => GeneratedSql.of("(", '{ $expr.`a, b, c` }, ")")
      case Parens.IfMulti => GeneratedSql.of('{ $expr.`(a, b, c)` })
      case Parens.Never   => GeneratedSql.of('{ $expr.`a, b, c` })

    final def `?, ?, ?`(expr: Expr[Columns[?]])(using Quotes): GeneratedSql = this match
      case Parens.Always  => GeneratedSql.of("(", '{ $expr.`?, ?, ?` }, ")")
      case Parens.IfMulti => GeneratedSql.of('{ $expr.`(?, ?, ?)` })
      case Parens.Never   => GeneratedSql.of('{ $expr.`?, ?, ?` })

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
