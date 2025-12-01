package oxygen.sql.generic.generation

import oxygen.core.syntax.functor.*
import oxygen.predef.core.*
import oxygen.sql.generic.parsing.TermTransformer
import scala.quoted.*

final class GeneratedFragment private (
    val generatedSql: GeneratedSql,
    val generatedInputEncoder: GeneratedInputEncoder,
) {

  def ++(that: GeneratedFragment): GeneratedFragment =
    GeneratedFragment(this.generatedSql ++ that.generatedSql, this.generatedInputEncoder ++ that.generatedInputEncoder)

  def wrapInParensIf(cond: Boolean): GeneratedFragment =
    if cond then GeneratedFragment.of("(", this, ")")
    else this

  def contramap(transform: TermTransformer.Transform): GeneratedFragment =
    GeneratedFragment(generatedSql, generatedInputEncoder.contramap(transform))

  def show(using Quotes): String =
    generatedSql.show

  def buildExpr(using Quotes): (GeneratedSql.Built, GeneratedInputEncoder.Built) =
    (generatedSql.buildExpr, generatedInputEncoder.buildExpr)

}
object GeneratedFragment {

  val empty: GeneratedFragment =
    GeneratedFragment(GeneratedSql.empty, GeneratedInputEncoder.empty)

  def sql(str: String | Expr[String]): GeneratedFragment =
    GeneratedFragment(GeneratedSql.single(str), GeneratedInputEncoder.empty)

  def both(sql: GeneratedSql, enc: GeneratedInputEncoder): GeneratedFragment =
    GeneratedFragment(sql, enc)

  def option(fragment: Option[GeneratedFragment]): GeneratedFragment =
    fragment.getOrElse(GeneratedFragment.empty)

  def flatten[S[_]: SeqOps](all: S[GeneratedFragment]): GeneratedFragment =
    GeneratedFragment(GeneratedSql.flatten(all.map(_.generatedSql)), GeneratedInputEncoder.flatten(all.map(_.generatedInputEncoder)))

  def indented(inner: GeneratedFragment, indent: String): GeneratedFragment =
    GeneratedFragment(GeneratedSql.indented(inner.generatedSql, indent), inner.generatedInputEncoder)

  def of(all: (String | Expr[String] | GeneratedFragment)*): GeneratedFragment =
    GeneratedFragment.flatten(
      Growable.many(all).map {
        case gen: GeneratedFragment => gen
        case str: String            => GeneratedFragment.sql(str)
        case str: Expr[String]      => GeneratedFragment.sql(str)
      },
    )

}
