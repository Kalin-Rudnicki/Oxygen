package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.schema.{InputEncoder, ResultDecoder, RowRepr}
import scala.annotation.unused
import scala.quoted.*
import scala.util.NotGiven

final class Macros(using quotes: Quotes) {

  def selectNoInputs[O: Type](select: Expr[SelectNoInput], f: Expr[Unit => Returning[O]]): Expr[QueryO[O]] =
    ParseContext.root("select without inputs") {
      for {
        (queryName, debug) <- select match {
          case '{ oxygen.sql.query.dsl.select.apply(${ Expr(queryName) }) }       => ParseResult.Success((queryName, false))
          case '{ oxygen.sql.query.dsl.select.apply(${ Expr(queryName) }).debug } => ParseResult.Success((queryName, true))
          case _                                                                  => ParseResult.error(select.toTerm, "Invalid `select` statement")
        }
        (_, _, sql, decoder) <- doStuffShared(select, f, false)

        sqlExpr = sql.buildExpr
        decoderExpr = decoder.buildExpr[O]

        query = '{
          new QueryO[O](
            ctx = QueryContext(
              queryName = ${ Expr(queryName) },
              sql = $sqlExpr,
              queryType = QueryContext.QueryType.Select,
            ),
            decoder = $decoderExpr,
          )
        }

        _ = if (debug)
          report.info(
            Contiguous(
              queryName,
              sqlExpr.showShortCode,
              decoderExpr.showAnsiCode,
              query.showAnsiCode,
            ).mkString(" \n", "\n \n ", "\n "),
            select,
          )

      } yield query
    }

  def selectInputs[I: Type, O: Type](select: Expr[SelectInput[I]], f: Expr[I => Returning[O]]): Expr[QueryIO[I, O]] =
    ParseContext.root("select with inputs") {
      for {
        (queryName, debug) <- select match {
          case '{ oxygen.sql.query.dsl.select.input[i](${ Expr(queryName) }) }       => ParseResult.Success((queryName, false))
          case '{ oxygen.sql.query.dsl.select.input[i](${ Expr(queryName) }).debug } => ParseResult.Success((queryName, true))
          case _                                                                     => ParseResult.error(select.toTerm, "Invalid `select` statement")
        }
        (_, repr, sql, decoder) <- doStuffShared(select, f, true)
        encoder <- generateInputs(repr)

        sqlExpr = sql.buildExpr
        encoderExpr = encoder.buildExpr[I]
        decoderExpr = decoder.buildExpr[O]

        query = '{
          new QueryIO[I, O](
            ctx = QueryContext(
              queryName = ${ Expr(queryName) },
              sql = $sqlExpr,
              queryType = QueryContext.QueryType.Select,
            ),
            encoder = $encoderExpr,
            decoder = $decoderExpr,
          )
        }

        _ = if (debug)
          report.info(
            Contiguous(
              queryName,
              sqlExpr.showShortCode,
              encoderExpr.showShortCode,
              decoderExpr.showShortCode,
              query.showAnsiCode,
            ).mkString(" \n", "\n \n ", "\n "),
            select,
          )

      } yield query
    }

  private def doStuffShared(
      select: Expr[Any],
      f: Expr[Any],
      shouldHaveInputs: Boolean,
  )(using ParseContext, Quotes): ParseResult[(Function, SelectQuery, GeneratedSql, GeneratedResultDecoder)] =
    for {
      rootFunction <- ParseContext.add("root function") { Function.parse(f.toTerm.underlying) }
      _ <-
        if (shouldHaveInputs) ParseResult.validate(rootFunction.params.nonEmpty)(select.toTerm, "function should have inputs")
        else ParseResult.validate(rootFunction.params.isEmpty)(select.toTerm, "function should not have inputs")
      initialRefs: RefMap =
        RefMap.empty.addList(
          rootFunction.params match {
            case p :: Nil => List(p.tree.symbol -> QueryReference.Input(p, None))
            case ps       => ps.zipWithIndex.map { case (p, i) => p.tree.symbol -> QueryReference.Input(p, i.some) }
          },
        )
      repr: SelectQuery <- ParseContext.add("repr") { SelectQuery.parse((rootFunction.body, initialRefs)) }

      sql: GeneratedSql <- generateSql(repr)
      decoder: GeneratedResultDecoder <-
        ParseContext.add("generating return row schemas") {
          for {
            _ <- ParseResult.validate(repr.returning.exprs.nonEmpty)(repr.returning.term, "Select returns nothing")
            res <- repr.returning.exprs.traverse { resultRowRepr(_) }
          } yield GeneratedResultDecoder.seq(res)
        }

    } yield (rootFunction, repr, sql, decoder)

  // FIX-PRE-MERGE (KR) :
  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other stuff
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def generateSql(repr: SelectQuery)(using ParseContext, Quotes): ParseResult[GeneratedSql] =
    ParseContext.add("generate sql") {
      for {

        selectSql: GeneratedSql <- ParseContext.add("select") {
          repr.returning.exprs.traverse { toSql(_) }.map { sqls =>
            GeneratedSql.of(
              "SELECT ",
              GeneratedSql.seq(sqls.toList.intersperse(GeneratedSql.const(",\n       "))),
            )
          }
        }

        fromSql: GeneratedSql = GeneratedSql.of(
          "\n    FROM ",
          GeneratedSql.single('{ ${ repr.from.schema }.schemaName }),
          ".",
          GeneratedSql.single('{ ${ repr.from.schema }.tableName }),
          " ",
          repr.from.param.name.camelToSnake,
        )

        joinSqls: GeneratedSql <- ParseContext.add("join") {
          repr.joins
            .traverse { j =>
              toSql(j.on).map { onSql =>
                GeneratedSql.of(
                  "\n    JOIN ",
                  GeneratedSql.single('{ ${ j.schema }.schemaName }),
                  ".",
                  GeneratedSql.single('{ ${ j.schema }.tableName }),
                  " ",
                  j.escapingParam.name.camelToSnake,
                  "\n      ON ",
                  onSql,
                )
              }
            }
            .map(GeneratedSql.seq)
        }

        whereSql: GeneratedSql <- ParseContext.add("where") {
          repr.where
            .traverse { w =>
              toSql(w.where).map { whereSql =>
                GeneratedSql.of(
                  "\n    WHERE ",
                  whereSql,
                )
              }
            }
            .map(GeneratedSql.option)
        }

      } yield GeneratedSql.of(
        selectSql,
        fromSql,
        joinSqls,
        whereSql,
      )
    }

  private def toSqlI(@unused expr: QueryExpr.InputLike, queryAgainst: QueryExpr.QueryLike, parens: Boolean): GeneratedSql = {
    val schema: Expr[RowRepr[?]] = queryAgainst.rowRepr

    parens match {
      case true =>
        GeneratedSql.single('{ $schema.columns.`(?, ?, ?)` })
      case false =>
        GeneratedSql.single('{ $schema.columns.`?, ?, ?` })
    }
  }

  private def toSqlQ(expr: QueryExpr.QueryLike, parens: Boolean): GeneratedSql = {
    val schema: Expr[RowRepr[?]] = expr.rowRepr

    parens match {
      case true =>
        GeneratedSql.single('{ $schema.columns.`(ref.a, ref.b, ref.c)`(${ Expr(expr.param.name.camelToSnake) }) })
      case false =>
        GeneratedSql.single('{ $schema.columns.`ref.a, ref.b, ref.c`(${ Expr(expr.param.name.camelToSnake) }) })
    }
  }

  private def toSql(expr: QueryExpr)(using ParseContext, Quotes): ParseResult[GeneratedSql] =
    expr match {
      case _: QueryExpr.InputLike           => ParseResult.error(expr.term, "not supported - input that does not reference table")
      case expr: QueryExpr.QueryLike        => ParseResult.Success(toSqlQ(expr, false))
      case QueryExpr.AndOr(_, lhs, op, rhs) =>
        for {
          lhs <- toSql(lhs).map(_.parensIf(lhs.isAndOr))
          rhs <- toSql(rhs).map(_.parensIf(rhs.isAndOr))
        } yield GeneratedSql.of(lhs, op.sqlPadded, rhs)
      case QueryExpr.Comp.Case1(_, lhs, op, rhs) =>
        ParseResult.Success(GeneratedSql.of(toSqlQ(lhs, true), op.sqlPadded, toSqlQ(rhs, true)))
      case QueryExpr.Comp.Case2(_, lhs, op, rhs) =>
        ParseResult.Success(GeneratedSql.of(toSqlQ(lhs, true), op.sqlPadded, toSqlI(rhs, lhs, true)))
      case QueryExpr.Comp.Case3(_, lhs, op, rhs) =>
        ParseResult.Success(GeneratedSql.of(toSqlI(lhs, rhs, true), op.sqlPadded, toSqlQ(rhs, true)))
    }

  private def generateInputs(repr: SelectQuery)(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
    ParseContext.add("generate input encoder") {
      for {
        selectEncoder: GeneratedInputEncoder <- ParseContext.add("select") {
          repr.returning.exprs.traverse(toInputs(_)).map(GeneratedInputEncoder.seq)
        }
        joinEncoder: GeneratedInputEncoder <- ParseContext.add("joins") {
          repr.joins.traverse { j => toInputs(j.on) }.map(GeneratedInputEncoder.seq)
        }
        whereEncoder: GeneratedInputEncoder <- ParseContext.add("where") {
          repr.where.traverse { w => toInputs(w.where) }.map(GeneratedInputEncoder.option)
        }
      } yield selectEncoder ++ joinEncoder ++ whereEncoder
    }

  private def toInputs(input: QueryExpr.InputLike, query: QueryExpr.QueryLike): GeneratedInputEncoder =
    GeneratedInputEncoder.single(input.param.tpe, input.fromInput, '{ ${ query.rowRepr }.encoder })

  private def toInputs(expr: QueryExpr)(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
    expr match {
      case _: QueryExpr.InputLike          => ParseResult.error(expr.term, "not supported - input that does not reference table")
      case _: QueryExpr.QueryLike          => ParseResult.Success(GeneratedInputEncoder.empty)
      case QueryExpr.AndOr(_, lhs, _, rhs) =>
        for {
          lhs <- toInputs(lhs)
          rhs <- toInputs(rhs)
        } yield lhs ++ rhs
      case _: QueryExpr.Comp.Case1              => ParseResult.Success(GeneratedInputEncoder.empty)
      case QueryExpr.Comp.Case2(_, lhs, _, rhs) => ParseResult.Success(toInputs(rhs, lhs))
      case QueryExpr.Comp.Case3(_, lhs, _, rhs) => ParseResult.Success(toInputs(lhs, rhs))
    }

  private def resultRowRepr(expr: QueryExpr)(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] = expr match
    case _: QueryExpr.InputLike    => ParseResult.error(expr.term, "returning an input is not supported")
    case expr: QueryExpr.QueryLike => ParseResult.Success(GeneratedResultDecoder.single(expr.term.tpe.widen, '{ ${ expr.rowRepr }.decoderWithColumns }))
    case _: QueryExpr.Binary       => ParseResult.Success(GeneratedResultDecoder.single(TypeRepr.of[Boolean], '{ RowRepr.boolean.decoderWithColumns }))

}
