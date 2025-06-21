package oxygen.sql.generic

import oxygen.meta.*
import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.schema.{InputEncoder, ResultDecoder, RowRepr, TableRepr}
import scala.annotation.{tailrec, unused}
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
  )(using ParseContext, Quotes): ParseResult[(Function, Repr, GeneratedSql, GeneratedResultDecoder)] =
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
      repr: Repr <- ParseContext.add("repr") { Repr.parse((rootFunction.body, initialRefs)) }

      sql: GeneratedSql <- generateSql(repr)
      decoder: GeneratedResultDecoder <-
        ParseContext.add("generating return row schemas") {
          repr.returning.exprs.traverse { resultRowRepr(_) }.map(GeneratedResultDecoder.nel)
        }

    } yield (rootFunction, repr, sql, decoder)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generate SQL
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class GeneratedSql private (sqls: Growable[Expr[String]]) {

    def ++(that: GeneratedSql): GeneratedSql = GeneratedSql(this.sqls ++ that.sqls)

    def parens: GeneratedSql = GeneratedSql.of("(", this, ")")
    def parensIf(cond: Boolean): GeneratedSql = if (cond) this.parens else this

    def buildExpr: Expr[String] =
      sqls.exprMkString

  }
  private object GeneratedSql {

    val empty: GeneratedSql = GeneratedSql(Growable.empty)
    def single(expr: Expr[String]): GeneratedSql = GeneratedSql(Growable.single(expr))
    def const(str: String): GeneratedSql = single(Expr(str))

    def option(sql: Option[GeneratedSql]): GeneratedSql = sql.getOrElse(empty)
    def seq(sqls: Seq[GeneratedSql]): GeneratedSql = GeneratedSql(Growable.many(sqls).flatMap(_.sqls))
    def nel(sqls: NonEmptyList[GeneratedSql]): GeneratedSql = seq(sqls.toList)
    def of(sqls: (String | GeneratedSql)*): GeneratedSql =
      seq(
        sqls.map {
          case sql: GeneratedSql => sql
          case str: String       => const(str)
        },
      )

  }

  private def generateSql(repr: Repr)(using ParseContext, Quotes): ParseResult[GeneratedSql] =
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Encoding Input
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class GeneratedInputEncoder(inputs: Growable[GeneratedInputEncoder.Elem]) {

    def ++(that: GeneratedInputEncoder): GeneratedInputEncoder =
      GeneratedInputEncoder(this.inputs ++ that.inputs)

    def buildExpr[I: Type]: Expr[InputEncoder[I]] =
      inputs.toContiguous match {
        case Contiguous() =>
          '{ InputEncoder.Empty }
        case Contiguous(single) =>
          single.buildExpr[I]
        case many =>
          '{ InputEncoder.ConcatAll[I](${ many.map(_.buildExpr[I]).seqToExpr }) }
      }

  }
  private object GeneratedInputEncoder {

    final case class Elem(
        typeRepr: TypeRepr,
        fromInput: Option[Expr[Any] => Expr[Any]],
        encoder: Expr[InputEncoder[?]],
    ) {

      def buildExpr[I: Type]: Expr[InputEncoder[I]] =
        fromInput match {
          case Some(fromInput) =>
            type MyI
            given Type[MyI] = typeRepr.asTypeOf
            val typedEncoder: Expr[InputEncoder[MyI]] = encoder.asExprOf[InputEncoder[MyI]]

            val f: Expr[I => MyI] =
              '{ (i: I) => ${ fromInput('i).asExprOf[MyI] } }

            '{ $typedEncoder.contramap[I]($f) }
          case None =>
            encoder.asExprOf[InputEncoder[I]]
        }

    }

    val empty: GeneratedInputEncoder = GeneratedInputEncoder(Growable.empty)
    def single(typeRepr: TypeRepr, fromInput: Option[Expr[Any] => Expr[Any]], encoder: Expr[InputEncoder[?]]): GeneratedInputEncoder =
      GeneratedInputEncoder(Growable.single(Elem(typeRepr, fromInput, encoder)))
    def option(opt: Option[GeneratedInputEncoder]): GeneratedInputEncoder = opt.getOrElse(empty)
    def seq(seq: Seq[GeneratedInputEncoder]): GeneratedInputEncoder = GeneratedInputEncoder(Growable.many(seq).flatMap(_.inputs))
    def nel(nel: NonEmptyList[GeneratedInputEncoder]): GeneratedInputEncoder = seq(nel.toList)

  }

  private def generateInputs(repr: Repr)(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
    ParseContext.add("generate input encoder") {
      for {
        selectEncoder: GeneratedInputEncoder <- ParseContext.add("select") {
          repr.returning.exprs.traverse(toInputs(_)).map(GeneratedInputEncoder.nel)
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decoding Result
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class GeneratedResultDecoder private (decoders: Growable[K0.Expressions.Elem[ResultDecoder, ?]]) {

    def ++(that: GeneratedResultDecoder): GeneratedResultDecoder = GeneratedResultDecoder(this.decoders ++ that.decoders)

    def buildExpr[O: Type]: Expr[ResultDecoder[O]] =
      decoders.toContiguous match {
        case Contiguous() =>
          '{ ResultDecoder.Empty }.asExprOf[ResultDecoder[O]]
        case Contiguous(single) =>
          single.expr.asExprOf[ResultDecoder[O]]
        case many =>
          type O2
          given tupGeneric: K0.ProductGeneric[O2] = K0.ProductGeneric.ofTuple[O2](many.map(_.bTpe.toTypeRepr).toList)
          given Type[O2] = tupGeneric.tpe
          val exprs: K0.Expressions[ResultDecoder, O2] = K0.Expressions(Type.of[ResultDecoder], tupGeneric.tpe, many)
          val instanceExpr: Expr[ResultDecoder[O2]] = DeriveProductResultDecoder(exprs).derive

          instanceExpr.asExprOf[ResultDecoder[O]]
      }

  }
  object GeneratedResultDecoder {

    val empty: GeneratedResultDecoder = GeneratedResultDecoder(Growable.empty)
    def single(tpe: TypeRepr, expr: Expr[ResultDecoder[?]]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.single(K0.Expressions.Elem(tpe.asTypeOf, expr)))
    def option(decoder: Option[GeneratedResultDecoder]): GeneratedResultDecoder = decoder.getOrElse(GeneratedResultDecoder.empty)
    def seq(decoders: Seq[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.many(decoders).flatMap(_.decoders))
    def nel(decoders: NonEmptyList[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder.seq(decoders.toList)

  }

  private def resultRowRepr(expr: QueryExpr)(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] = expr match
    case _: QueryExpr.InputLike    => ParseResult.error(expr.term, "returning an input is not supported")
    case expr: QueryExpr.QueryLike => ParseResult.Success(GeneratedResultDecoder.single(expr.term.tpe.widen, '{ ${ expr.rowRepr }.decoderWithColumns }))
    case _: QueryExpr.Binary       => ParseResult.Success(GeneratedResultDecoder.single(TypeRepr.of[Boolean], '{ RowRepr.boolean.decoderWithColumns }))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension (self: Term)
    private def simplify: Term = self.underlying match
      case Typed(term, _) => term.simplify
      case _              => self

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      DSL
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class Repr(
      from: Repr.From,
      joins: List[Repr.Join],
      where: Option[Repr.Where],
      returning: Repr.Returning,
  ) {

    def show(rootParams: List[Function.Param]): String = {
      val paramStrs: List[String] =
        if (rootParams.nonEmpty) "INPUTS:" +: (rootParams.map { p => s"  ${p.name.cyanFg}: ${p.tpe.show}," } :+ "")
        else Nil

      val allStrs: List[List[String]] =
        List(
          paramStrs,
          List(returning.show, from.show),
          joins.map(_.show),
          where.map(_.show).toList,
        )

      allStrs.flatten.mkString("\n")
    }

  }
  private object Repr extends Parser[(Term, RefMap), Repr] {

    @tailrec
    private def joinLoop(
        stack: List[Join],
        funct: String,
        refs: RefMap,
        term: Term,
    )(using ParseContext, Quotes): ParseResult.Known[(List[Join], String, RefMap, Term)] =
      Join.parse((term, refs)) match
        case ParseResult.Success((join, funct, refs, term)) => joinLoop(join :: stack, funct, refs, term)
        case error: ParseResult.Error                       => error
        case _: ParseResult.Unknown                         => ParseResult.Success((stack.reverse, funct, refs, term))

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[Repr] =
      for {
        (term, refs) <- ParseResult.Success(input)
        (from, fromFunct, refs, fromRhs) <- ParseContext.add("`from`") {
          From.parse((term, refs))
        }
        (joins, joinFunct, refs, joinRhs) <- ParseContext.add("`join`s") {
          joinLoop(Nil, fromFunct, refs, fromRhs)
        }
        (where, whereFunct, whereRhs) <- ParseContext.add("`where`") {
          Where
            .parse((joinRhs, refs))
            .map { case (where, whereFunct, whereRhs) => (where.some, whereFunct, whereRhs) }
            .orElse { (None, joinFunct, joinRhs) }
        }
        _ <- ParseResult.validate(whereFunct == "map")(whereRhs, "expected to be at final `map`")
        returning <- ParseContext.add("`returning`") {
          Returning.parse((whereRhs, refs))
        }
      } yield Repr(from, joins, where, returning)

    final case class From(param: Function.Param, schema: Expr[TableRepr[?, ?]]) {

      def show: String =
        s"  FROM ${param.tpe.show} ${param.name.greenFg}"

    }
    object From extends Parser[(Term, RefMap), (From, String, RefMap, Term)] {

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[(From, String, RefMap, Term)] =
        for {
          (term, refs) <- ParseResult.Success(input)

          (f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { functionCall.parse(term) }
          schema <- f1Lhs.asExpr match {
            case '{ from[a](using $schema) } => ParseResult.Success(schema)
            case _                           => ParseResult.error(f1Lhs, "invalid `from` invocation")
          }
          p1 <- f1Function.parseSingleParam
          f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError

          newRefs = refs.add(p1.tree.symbol -> QueryReference.Query(p1, schema, true))

        } yield (From(p1, schema), f1Name, newRefs, f1Function.body)

    }

    final case class Join(escapingParam: Function.Param, onParam: Function.Param, on: QueryExpr, schema: Expr[TableRepr[?, ?]]) {

      def show: String =
        s"  JOIN ${escapingParam.tpe.show} ${escapingParam.name.greenFg}\n    ON ${on.show}"

    }
    object Join extends Parser[(Term, RefMap), (Join, String, RefMap, Term)] {

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[(Join, String, RefMap, Term)] =
        for {
          (term, refs) <- ParseResult.Success(input)

          (f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { functionCall.parse(term) }
          (f2Lhs, f2Name, f2Function) <- ParseContext.add("function 2") { functionCall.parse(f1Lhs) }
          schema <- f2Lhs.asExpr match {
            case '{ join[a](using $schema) } => ParseResult.Success(schema)
            case _                           => ParseResult.unknown(f2Lhs, "invalid `join` invocation")
          }
          p1 <- f1Function.parseSingleParam
          p2 <- f2Function.parseSingleParam
          f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError
          _ <- functionNames.withFilter.parse(f2Name).unknownAsError

          newRefs = refs.add(
            p1.tree.symbol -> QueryReference.Query(p1, schema, false),
            p2.tree.symbol -> QueryReference.Query(p2, schema, false),
          )

          onExpr <- RawQueryExpr.parse((f2Function.body.simplify, newRefs)).unknownAsError
          onExpr <- QueryExpr.parse(onExpr)

        } yield (Join(p1, p2, onExpr, schema), f1Name, newRefs, f1Function.body)

    }

    final case class Where(where: QueryExpr) {

      def show: String =
        s"  WHERE ${where.show}"

    }
    object Where extends Parser[(Term, RefMap), (Where, String, Term)] {

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[(Where, String, Term)] =
        for {
          (term, refs) <- ParseResult.Success(input)

          (f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { functionCall.parse(term) }
          (f2Lhs, f2Name, f2Function) <- ParseContext.add("function 2") { functionCall.parse(f1Lhs) }
          _ <- f2Lhs.asExpr match {
            case '{ where } => ParseResult.Success(())
            case _          => ParseResult.unknown(f2Lhs, "invalid `where` invocation")
          }
          _ <- f1Function.parseEmptyParams
          _ <- f2Function.parseEmptyParams
          f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError
          _ <- functionNames.withFilter.parse(f2Name).unknownAsError

          whereExpr <- RawQueryExpr.parse((f2Function.body.simplify, refs)).unknownAsError
          whereExpr <- QueryExpr.parse(whereExpr)

        } yield (Where(whereExpr), f1Name, f1Function.body)

    }

    final case class Returning(exprs: NonEmptyList[QueryExpr]) {

      def show: String =
        s"SELECT  ${exprs.map(_.show).mkString(" , ")}"

    }
    object Returning extends Parser[(Term, RefMap), Returning] {

      override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[Returning] =
        for {
          (term, refs) <- ParseResult.Success(input)
          terms = term match
            case tupleApply(args) => args
            case _                => NonEmptyList.one(term)
          exprs <- terms.traverse(t => RawQueryExpr.parse((t, refs)))
          exprs <- exprs.traverse(t => QueryExpr.parse(t))
        } yield Returning(exprs)

    }

  }

}
