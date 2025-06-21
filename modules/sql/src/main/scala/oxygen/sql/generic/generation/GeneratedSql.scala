package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final class GeneratedSql private (
    private val parts: Growable[Expr[String]],
) {

  def ++(that: GeneratedSql): GeneratedSql =
    GeneratedSql(this.parts ++ that.parts)

  def buildExpr(using Quotes): GeneratedSql.Built =
    GeneratedSql.Built(parts.exprMkString)

  def show(using Quotes): String =
    "=====| Generated Sql |=====" +
      parts
        .map {
          _.evalEither match {
            case Right(value) => s"\n[Const Str]:\n$value"
            case Left(value)  => s"\n[Str Expr]:\n${value.showAnsiCode}"
          }
        }
        .to[Seq]
        .mkString

}
object GeneratedSql {

  final case class Built(sql: Expr[String])

  val empty: GeneratedSql = GeneratedSql(Growable.empty)

  def single(str: String | Expr[String])(using Quotes): GeneratedSql = str match
    case str: String       => GeneratedSql(Growable.single(Expr(str)))
    case str: Expr[String] => GeneratedSql(Growable.single(str))

  def option(fragment: Option[GeneratedSql]): GeneratedSql = fragment.getOrElse(GeneratedSql.empty)

  def flatten[S[_]: SeqOps](all: S[GeneratedSql]): GeneratedSql = GeneratedSql(Growable.manyFlatMapped(all)(_.parts))

  def of(all: (String | Expr[String])*)(using Quotes): GeneratedSql =
    GeneratedSql(
      Growable.manyMapped(all) {
        case str: String        => Expr(str)
        case expr: Expr[String] => expr
      },
    )

}
