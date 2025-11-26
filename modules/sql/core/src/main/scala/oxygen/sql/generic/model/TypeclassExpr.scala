package oxygen.sql.generic.model

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.parsing.TermTransformer
import oxygen.sql.schema as S
import scala.quoted.*

object TypeclassExpr {

  final class TableRepr(ctxExpr: Quotes ?=> Expr[S.TableRepr[?]]) {

    def expr(using quotes: Quotes): Expr[S.TableRepr[?]] = ctxExpr(using quotes)

    def tableRowRepr: TypeclassExpr.RowRepr =
      TypeclassExpr.RowRepr { '{ $expr.rowRepr } }

    private def pkRowReprShared(fullTerm: Term, field: String): TypeclassExpr.RowRepr =
      TypeclassExpr.RowRepr {
        type T
        given Type[T] = fullTerm.tpe.asTypeOf

        expr.toTerm.select(field).select("rowRepr").asExprOf[S.RowRepr[T]]
      }

    def pkRowRepr(fullTerm: Term): TypeclassExpr.RowRepr = pkRowReprShared(fullTerm, "pk")
    def npkRowRepr(fullTerm: Term): TypeclassExpr.RowRepr = pkRowReprShared(fullTerm, "npk")

    private def getPKShared(in: Term, field: String)(using Quotes): Term =
      expr.toTerm.select(field).select("get").appliedTo(in)

    def getPK(in: Term)(using Quotes): Term = getPKShared(in, "pk")
    def getNPK(in: Term)(using Quotes): Term = getPKShared(in, "npk")

    def tableRef(using Quotes): Expr[String] = '{ $expr.ref }

    def tableNameFirstChar(using Quotes): Expr[String] = '{ $expr.tableName.head.toString }

    def show(using Quotes): String = expr.showAnsiCode

  }
  object TableRepr {

    def wrapTerm(term: Term): TypeclassExpr.TableRepr =
      TypeclassExpr.TableRepr(term.asExpr.asInstanceOf[Expr[S.TableRepr[?]]])

  }

  final class RowRepr(ctxExpr: Quotes ?=> Expr[S.RowRepr[?]]) {

    def expr(using quotes: Quotes): Expr[S.RowRepr[?]] = ctxExpr(using quotes)

    def optional: TypeclassExpr.RowRepr =
      TypeclassExpr.RowRepr { '{ $expr.optional } }

    def productSchemaField(term: Term, field: String): TypeclassExpr.RowRepr =
      TypeclassExpr.RowRepr {
        type T
        given Type[T] = term.tpe.widen.asTypeOf

        '{ $expr.unsafeChild[T](${ Expr(field) }) }
      }

    def optionSchemaGet(term: Term): TypeclassExpr.RowRepr =
      TypeclassExpr.RowRepr {
        type T
        given Type[T] = term.tpe.widen.asTypeOf

        '{ $expr.unsafeRequired[T] }
      }

    def columns: TypeclassExpr.Columns =
      TypeclassExpr.Columns { '{ $expr.columns } }

    def inputEncoder: TypeclassExpr.InputEncoder =
      TypeclassExpr.InputEncoder { '{ $expr.encoder } }

    def resultDecoder: TypeclassExpr.ResultDecoder =
      TypeclassExpr.ResultDecoder { '{ $expr.decoder } }

    def show(using Quotes): String = expr.showAnsiCode

  }
  object RowRepr {

    val int: TypeclassExpr.RowRepr = TypeclassExpr.RowRepr { '{ S.RowRepr.int } }
    val long: TypeclassExpr.RowRepr = TypeclassExpr.RowRepr { '{ S.RowRepr.long } }
    val boolean: TypeclassExpr.RowRepr = TypeclassExpr.RowRepr { '{ S.RowRepr.boolean } }
    val uuid: TypeclassExpr.RowRepr = TypeclassExpr.RowRepr { '{ S.RowRepr.uuid } }
    val instant: TypeclassExpr.RowRepr = TypeclassExpr.RowRepr { '{ S.RowRepr.instant } }
    val string: TypeclassExpr.RowRepr = TypeclassExpr.RowRepr { '{ S.RowRepr.string } }

  }

  final class Columns(ctxExpr: Quotes ?=> Expr[S.Columns[?]]) {

    def expr(using quotes: Quotes): Expr[S.Columns[?]] = ctxExpr(using quotes)

    def `ref.a, ref.b, ref.c`(ctx: GenerationContext.Parens, ref: Expr[String])(using Quotes): GeneratedSql = ctx match
      case GenerationContext.Parens.Always  => GeneratedSql.of("(", '{ $expr.`ref.a, ref.b, ref.c`($ref) }, ")")
      case GenerationContext.Parens.IfMulti => GeneratedSql.of('{ $expr.`(ref.a, ref.b, ref.c)`($ref) })
      case GenerationContext.Parens.Never   => GeneratedSql.of('{ $expr.`ref.a, ref.b, ref.c`($ref) })

    def `a, b, c`(ctx: GenerationContext.Parens)(using Quotes): GeneratedSql = ctx match
      case GenerationContext.Parens.Always  => GeneratedSql.of("(", '{ $expr.`a, b, c` }, ")")
      case GenerationContext.Parens.IfMulti => GeneratedSql.of('{ $expr.`(a, b, c)` })
      case GenerationContext.Parens.Never   => GeneratedSql.of('{ $expr.`a, b, c` })

    def `?, ?, ?`(ctx: GenerationContext.Parens)(using Quotes): GeneratedSql = ctx match
      case GenerationContext.Parens.Always  => GeneratedSql.of("(", '{ $expr.`?, ?, ?` }, ")")
      case GenerationContext.Parens.IfMulti => GeneratedSql.of('{ $expr.`(?, ?, ?)` })
      case GenerationContext.Parens.Never   => GeneratedSql.of('{ $expr.`?, ?, ?` })

    def exprSeqQMark(using Quotes): Expr[ArraySeq[String]] =
      '{ $expr.columns.map(_ => "?") }

    def exprSeqNames(using Quotes): Expr[ArraySeq[String]] =
      '{ $expr.columns.map(_.name) }

    def exprSeqRefNames(refStr: String)(using Quotes): Expr[ArraySeq[String]] =
      '{ $expr.columns.map(c => s"${${ Expr(refStr) }}.${c.name}") }

  }

  final class InputEncoder(ctxExpr: Quotes ?=> Expr[S.InputEncoder[?]]) {

    def expr(using quotes: Quotes): Expr[S.InputEncoder[?]] = ctxExpr(using quotes)

    def optional: TypeclassExpr.InputEncoder =
      TypeclassExpr.InputEncoder { '{ $expr.optional } }

    def constEncoder(term: Term): TypeclassExpr.InputEncoder =
      TypeclassExpr.InputEncoder {
        type T
        given Type[T] = term.tpe.widen.asTypeOf

        val termExpr: Expr[T] = term.asExprOf[T]

        '{ S.InputEncoder.Const[T](${ expr.asExprOf[S.InputEncoder[T]] }, $termExpr) }
      }

    def contramap(transform: TermTransformer.Transform): TypeclassExpr.InputEncoder =
      TypeclassExpr.InputEncoder {
        type In
        type Out
        given Type[In] = transform.inTpe.asTypeOf
        given Type[Out] = transform.outTpe.asTypeOf

        val typed: Expr[S.InputEncoder[Out]] = expr.asExprOf[S.InputEncoder[Out]]
        '{ $typed.contramap[In](${ transform.convertExprF[In, Out] }) }
      }

    def ~[T: Type](that: TypeclassExpr.InputEncoder): TypeclassExpr.InputEncoder = {
      TypeclassExpr.InputEncoder { '{ ${ this.expr.asExprOf[S.InputEncoder[T]] } ~ ${ that.expr.asExprOf[S.InputEncoder[T]] } } }
    }

    def typedAs(tpe: TypeRepr): TypeclassExpr.InputEncoder =
      TypeclassExpr.InputEncoder {
        type T
        given Type[T] = tpe.asTypeOf
        expr.asExprOf[S.InputEncoder[T]]
      }
    def typedAs[T: Type]: TypeclassExpr.InputEncoder =
      TypeclassExpr.InputEncoder { expr.asExprOf[S.InputEncoder[T]] }

  }
  object InputEncoder {

    val isNullEncoder: TypeclassExpr.InputEncoder = TypeclassExpr.InputEncoder { '{ S.InputEncoder.isNullEncoder } }

  }

  final class ResultDecoder(ctxExpr: Quotes ?=> Expr[S.ResultDecoder[?]]) {

    def expr(using quotes: Quotes): Expr[S.ResultDecoder[?]] = ctxExpr(using quotes)

    def optional: TypeclassExpr.ResultDecoder =
      TypeclassExpr.ResultDecoder { '{ $expr.optional } }

  }

}
