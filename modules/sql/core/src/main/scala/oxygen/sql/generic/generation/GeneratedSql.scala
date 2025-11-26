package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final class GeneratedSql private (
    private val parts: Growable[GeneratedSql.Part],
) {

  def ++(that: GeneratedSql): GeneratedSql =
    GeneratedSql(this.parts ++ that.parts)

  def buildExpr(using Quotes): GeneratedSql.Built =
    GeneratedSql.Built(parts.map(_.expr).exprMkString)

  def show(using Quotes): String =
    "=====| Generated Sql |=====" +
      parts
        .map {
          _.expr.evalEither match {
            case Right(value) => s"\n[Const Str]:\n$value"
            case Left(value)  => s"\n[Str Expr]:\n${value.showAnsiCode}"
          }
        }
        .to[Seq]
        .mkString

}
object GeneratedSql {

  private final class Part private (_expr: Quotes => Expr[String]) {
    def expr(using quotes: Quotes): Expr[String] = _expr(quotes)
  }
  private object Part {

    def make(f: Quotes ?=> Expr[String]): Part = new Part(q => f(using q))

    def simple(str: Quotes ?=> (String | Expr[String])): Part =
      Part.make { q ?=>
        str(using q) match {
          case str: String       => Expr(str)
          case str: Expr[String] => str
        }
      }

  }

  final case class Built(sql: Expr[String])

  val empty: GeneratedSql = GeneratedSql(Growable.empty)

  def single(str: Quotes ?=> (String | Expr[String])): GeneratedSql =
    GeneratedSql(Growable.single(Part.simple(str)))

  def option(fragment: Option[GeneratedSql]): GeneratedSql = fragment.getOrElse(GeneratedSql.empty)

  def flatten[S[_]: SeqOps](all: S[GeneratedSql]): GeneratedSql = GeneratedSql(Growable.manyFlatMapped(all)(_.parts))

  def indented(inner: GeneratedSql, indent: String): GeneratedSql = {
    val part: GeneratedSql.Part =
      GeneratedSql.Part.make {
        '{
          val innerString: String = ${ inner.buildExpr.sql }
          ${ Expr(indent) } + innerString.replaceAll("\n", ${ Expr("\n" + indent) })
        }
      }

    GeneratedSql(Growable.single(part))
  }

  def of(all: (Quotes ?=> (String | Expr[String]))*): GeneratedSql =
    GeneratedSql(Growable.manyMapped(all)(Part.simple))

}
