package oxygen.sql.generic

import oxygen.core.typeclass.{Functor, Traverse}
import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.predef.meta.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.*
import oxygen.sql.schema.{InputEncoder, ResultDecoder, RowRepr, TableRepr}
import scala.annotation.{tailrec, unused}
import scala.collection.mutable
import scala.util.NotGiven

final class Macros[Q <: Quotes](val k0: K0[Q]) {
  import k0.meta.*
  import k0.meta.flatTerms.*
  import k0.meta.given

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
              sqlExpr.showSimple,
              decoderExpr.showAnsi,
              query.showAnsi,
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
              sqlExpr.showSimple,
              encoderExpr.showSimple,
              decoderExpr.showSimple,
              query.showAnsi,
            ).mkString(" \n", "\n \n ", "\n "),
            select,
          )

      } yield query
    }

  private def doStuffShared(
      select: Expr[Any],
      f: Expr[Any],
      shouldHaveInputs: Boolean,
  )(using ParseContext): ParseResult[(Function, Repr, GeneratedSql, GeneratedResultDecoder)] =
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

    def buildExpr: Expr[String] = {
      def addAll(builder: Expr[mutable.StringBuilder]): Expr[String] =
        Expr.block(
          sqls.toContiguous.toList.map { expr => '{ $builder.append($expr) } },
          '{ $builder.result() },
        )

      '{
        val builder = mutable.StringBuilder(128)
        ${ addAll('builder) }
      }
    }

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

  private def generateSql(repr: Repr)(using ParseContext): ParseResult[GeneratedSql] =
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

  private def toSql(expr: QueryExpr)(using ParseContext): ParseResult[GeneratedSql] =
    expr match {
      case _: QueryExpr.InputLike    => ParseResult.error(expr.term, "not supported - input that does not reference table")
      case expr: QueryExpr.QueryLike => ParseResult.Success(toSqlQ(expr, false))
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
          '{ InputEncoder.ConcatAll[I](${ many.map(_.buildExpr[I]).flattenExprs }) }
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
            given Type[MyI] = typeRepr.asTyped
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

  private def generateInputs(repr: Repr)(using ParseContext): ParseResult[GeneratedInputEncoder] =
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

  private def toInputs(expr: QueryExpr)(using ParseContext): ParseResult[GeneratedInputEncoder] =
    expr match {
      case _: QueryExpr.InputLike => ParseResult.error(expr.term, "not supported - input that does not reference table")
      case _: QueryExpr.QueryLike => ParseResult.Success(GeneratedInputEncoder.empty)
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

  final case class GeneratedResultDecoder private (decoders: Growable[GeneratedResultDecoder.Elem]) {

    def ++(that: GeneratedResultDecoder): GeneratedResultDecoder = GeneratedResultDecoder(this.decoders ++ that.decoders)

    def buildExpr[O: Type]: Expr[ResultDecoder[O]] =
      decoders.toContiguous match {
        case Contiguous(single) =>
          single.decoder.asExprOf[ResultDecoder[O]]
        case Contiguous() =>
          '{ ResultDecoder.Empty }.asExprOf[ResultDecoder[O]]
        case many =>
          val insts: k0.ValExpressions[ResultDecoder] = k0.ValExpressions.unsafeMake(many.map(_.toValExprElem))
          k0.ProductGeneric.tupleInstance(ResultDecoder, insts)._2.asExprOf[ResultDecoder[O]]
      }

  }
  object GeneratedResultDecoder {

    final case class Elem(
        typeRepr: TypeRepr,
        decoder: Expr[ResultDecoder[?]],
    ) {

      def toValExprElem: k0.ValExpressions.Elem[ResultDecoder, ?] =
        k0.ValExpressions.Elem.unsafeMake(decoder, typeRepr.asTyped)

    }

    val empty: GeneratedResultDecoder = GeneratedResultDecoder(Growable.empty)
    def single(tpe: TypeRepr, expr: Expr[ResultDecoder[?]]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.single(Elem(tpe, expr)))
    def option(decoder: Option[GeneratedResultDecoder]): GeneratedResultDecoder = decoder.getOrElse(GeneratedResultDecoder.empty)
    def seq(decoders: Seq[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder(Growable.many(decoders).flatMap(_.decoders))
    def nel(decoders: NonEmptyList[GeneratedResultDecoder]): GeneratedResultDecoder = GeneratedResultDecoder.seq(decoders.toList)

  }

  private def resultRowRepr(expr: QueryExpr)(using ParseContext): ParseResult[GeneratedResultDecoder] = expr match
    case _: QueryExpr.InputLike    => ParseResult.error(expr.term, "returning an input is not supported")
    case expr: QueryExpr.QueryLike => ParseResult.Success(GeneratedResultDecoder.single(expr.term.tpe.widen, '{ ${ expr.rowRepr }.decoderWithColumns }))
    case _: QueryExpr.Binary       => ParseResult.Success(GeneratedResultDecoder.single(TypeRepr.of[Boolean], '{ RowRepr.boolean.decoderWithColumns }))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def productSchemaField(term: Term, field: String, schema: Expr[RowRepr[?]]): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTyped

    '{ $schema.unsafeChild[T](${ Expr(field) }) }
  }

  private def optionSchemaGet(term: Term, schema: Expr[RowRepr[?]]): Expr[RowRepr[?]] = {
    type T
    given Type[T] = term.tpe.widen.asTyped

    '{ $schema.unsafeRequired[T] }
  }

  extension (self: Term)
    private def simplify: Term = self.underlying match
      case Tree.Statement.Term.Typed(term, _) => term.simplify
      case _                                  => self

  private trait Parser[-A, +B] {

    def parse(input: A)(using ParseContext): ParseResult[B]

    object required {
      def unapply(input: A)(using attempting: ParseContext): Some[ParseResult[B]] =
        Some(parse(input))
    }

    object optional {
      def unapply(input: A)(using attempting: ParseContext): Option[ParseResult.Known[B]] =
        parse(input).toKnown
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      DSL
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class ParseContext private (context: Growable[String]) {
    def :+(ctx: String): ParseContext = ParseContext(context :+ ctx)
    override def toString: String = context.toContiguous.mkString(" -> ")
  }
  private object ParseContext {

    def root[A](attempting: String)(f: ParseContext ?=> ParseResult[A])(using NotGiven[ParseContext]): A =
      f(using ParseContext(Growable.single(attempting))).getOrReport

    def add[A](ctx: String)(f: ParseContext ?=> A)(using parent: ParseContext): A =
      f(using parent :+ ctx)

  }

  private given parseResultFunctor: [F[_]] => (functor: Functor[F]) => Traverse[F, ParseResult] =
    new Traverse[F, ParseResult] {
      override def traverse[A, B](self: F[A])(f: A => ParseResult[B]): ParseResult[F[B]] =
        ParseResult.fromEither(Traverse.functorEither[F, ParseResult.NotSuccess](using functor).traverse(self)(f(_).toEither))
    }

  private sealed trait ParseResult[+A] {

    def map[B](f: A => B): ParseResult[B]
    def flatMap[B](f: A => ParseResult[B]): ParseResult[B]

    final def toSuccess: Option[A] = this match
      case ParseResult.Success(value) => value.some
      case _: ParseResult.Error       => None
      case _: ParseResult.Unknown     => None

    final def toKnown: Option[ParseResult.Known[A]] = this match
      case known: ParseResult.Known[A] => known.some
      case _: ParseResult.Unknown      => None

    final def getOrReport: A = this match
      case ParseResult.Success(value)                     => value
      case ParseResult.Error(tree, attempting, message)   => report.errorAndAbort(s"Error attempting $attempting : $message\n\n" + tree.showDetailed("tree"))
      case ParseResult.Unknown(tree, attempting, message) => report.errorAndAbort(s"Unknown tree attempting $attempting : $message\n\n" + tree.showDetailed("tree"))

    final def toEither: Either[ParseResult.NotSuccess, A] = this match
      case ParseResult.Success(value)         => value.asRight
      case notSuccess: ParseResult.NotSuccess => notSuccess.asLeft

    final def orElse[A2 >: A](default: => A2): ParseResult.Known[A2] =
      this.toKnown.getOrElse(ParseResult.Success(default))

    final def unknownAsError: ParseResult.Known[A] = this match
      case success: ParseResult.Success[A]                => success
      case error: ParseResult.Error                       => error
      case ParseResult.Unknown(tree, attempting, message) => ParseResult.Error(tree, attempting, message)

    final def errorAsUnknown: ParseResult[A] = this match
      case success: ParseResult.Success[A]              => success
      case unknown: ParseResult.Unknown                 => unknown
      case ParseResult.Error(tree, attempting, message) => ParseResult.Unknown(tree, attempting, message)

  }
  private object ParseResult {

    def fromEither[A](either: Either[ParseResult.NotSuccess, A]): ParseResult[A] = either match
      case Right(value) => ParseResult.Success(value)
      case Left(value)  => value

    def validate(cond: Boolean)(tree: Tree, message: String)(using ParseContext): ParseResult.Known[Unit] =
      if (cond) ParseResult.Success(())
      else ParseResult.error(tree, message)

    sealed trait Known[+A] extends ParseResult[A] {

      override def map[B](f: A => B): ParseResult[B] = this match
        case ParseResult.Success(value) => ParseResult.Success(f(value))
        case error: ParseResult.Error   => error

      override def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = this match
        case ParseResult.Success(value) => f(value)
        case error: ParseResult.Error   => error

    }

    sealed trait NotSuccess extends ParseResult[Nothing]

    final case class Success[+A](value: A) extends ParseResult.Known[A]

    final case class Error(tree: Tree, attempting: ParseContext, message: String) extends ParseResult.Known[Nothing], NotSuccess

    final case class Unknown(tree: Tree, attempting: ParseContext, message: String) extends NotSuccess {
      override def map[B](f: Nothing => B): ParseResult[B] = this
      override def flatMap[B](f: Nothing => ParseResult[B]): ParseResult[B] = this
    }

    def success[A](value: A): ParseResult[A] = Success(value)
    def error(tree: Tree, message: String)(using attempting: ParseContext): Error = Error(tree, attempting, message)
    def unknown(tree: Tree, message: String)(using attempting: ParseContext): Unknown = Unknown(tree, attempting, message)

  }

  private enum QueryReference {

    val param: Function.Param

    case Input(param: Function.Param, idx: Option[Int])
    case Query(param: Function.Param, schema: Expr[TableRepr[?, ?]], isRoot: Boolean)

    final def show: String = this match
      case QueryReference.Input(param, idx)           => param.name.greenFg.toString
      case QueryReference.Query(param, schema, true)  => param.name.hexFg("#7EB77F").toString
      case QueryReference.Query(param, schema, false) => param.name.hexFg("#FFADC6").toString

  }

  private sealed trait BinOp {
    val sql: String
    val scala: String

    def show: String

    final def sqlPadded: String = s" $sql "

  }
  private object BinOp extends Enum.Companion[BinOp] {

    enum Comp(final val sql: String, final val scala: String) extends BinOp {
      case `==` extends Comp("=", "==")
      case `!=` extends Comp("!=", "!=")
      case `<` extends Comp("<", "<")
      case `<=` extends Comp("<=", "<=")
      case `>` extends Comp(">", ">")
      case `>=` extends Comp(">=", ">=")

      final def show: String = sql.hexFg("#E6C120").toString

    }

    enum AndOr(final val sql: String, final val scala: String) extends BinOp {
      case `&&` extends AndOr("AND", "&&")
      case `||` extends AndOr("OR", "||")

      final def show: String = sql.hexFg("#FF6666").toString

    }

    override def values: Array[BinOp] = Comp.values ++ AndOr.values

    object Sql extends EnumMap[String](op => NonEmptyList.one(op.sql))
    object Scala extends EnumMap[String](op => NonEmptyList.one(op.scala))

  }

  private sealed trait QueryExpr {

    val term: Term

    final def show: String = this match
      case QueryExpr.InputLike.Ref(_, param, _)                 => param.name.hexFg("#F71735").toString
      case QueryExpr.InputLike.ProductField(_, inner, field, _) => s"${inner.show}.${field.cyanFg}"
      case QueryExpr.QueryLike.Ref(_, param, _, true)           => param.name.hexFg("#A81ADB").toString
      case QueryExpr.QueryLike.Ref(_, param, _, false)          => param.name.hexFg("#540D6E").toString
      case QueryExpr.QueryLike.ProductField(_, inner, field)    => s"${inner.show}.${field.cyanFg}"
      case QueryExpr.QueryLike.OptionGet(_, inner)              => s"${inner.show}.${"get".blueFg}"
      case QueryExpr.AndOr(_, lhs, op, rhs)                     => s"${lhs.showWrapAndOr} ${op.show} ${rhs.showWrapAndOr}"
      case comp: QueryExpr.Comp                                 => s"${comp.lhs.show} ${comp.op.show} ${comp.rhs.show}"

    final def isAndOr: Boolean = this match
      case _: QueryExpr.AndOr => true
      case _                  => false

    final def showWrapAndOr: String = this match
      case _: QueryExpr.AndOr => s"($show)"
      case _                  => show

  }
  private object QueryExpr extends Parser[RawQueryExpr, QueryExpr] {

    sealed trait Unary extends QueryExpr {
      val param: Function.Param
    }
    object Unary extends Parser[RawQueryExpr.Unary, QueryExpr.Unary] {

      override def parse(expr: RawQueryExpr.Unary)(using ParseContext): ParseResult[QueryExpr.Unary] =
        expr match {
          case RawQueryExpr.Ref(term, QueryReference.Query(param, schema, isRoot)) =>
            ParseResult.Success(QueryExpr.QueryLike.Ref(term, param, schema, isRoot))
          case RawQueryExpr.Ref(term, QueryReference.Input(param, idx)) =>
            ParseResult.Success(QueryExpr.InputLike.Ref(term, param, idx))
          case RawQueryExpr.ProductField(term, ref, field, access) =>
            parse(ref).map {
              case ref: QueryExpr.QueryLike => QueryExpr.QueryLike.ProductField(term, ref, field)
              case ref: QueryExpr.InputLike => QueryExpr.InputLike.ProductField(term, ref, field, access)
            }
          case RawQueryExpr.OptionGet(term, ref) =>
            parse(ref).flatMap {
              case ref: QueryExpr.QueryLike => ParseResult.Success(QueryExpr.QueryLike.OptionGet(term, ref))
              case _: QueryExpr.InputLike   => ParseResult.error(term, "not supported: `Option.get` on input")
            }
        }

    }

    sealed trait InputLike extends Unary {
      val idx: Option[Int]
      def fromInput: Option[Expr[Any] => Expr[Any]]
    }
    object InputLike {

      final case class Ref(term: Term, param: Function.Param, idx: Option[Int]) extends InputLike {
        override def fromInput: Option[Expr[Any] => Expr[Any]] = param.fromInput
      }

      final case class ProductField(term: Term, inner: InputLike, field: String, access: Expr[Any] => Expr[Any]) extends InputLike {
        override val param: Function.Param = inner.param
        override val idx: Option[Int] = inner.idx
        override def fromInput: Option[Expr[Any] => Expr[Any]] = inner.fromInput match
          case Some(innerF) => Some { i => access(innerF(i)) }
          case None         => access.some
      }

    }

    sealed trait QueryLike extends Unary {
      val tableSchema: Expr[TableRepr[?, ?]]
      def rowRepr: Expr[RowRepr[?]]
      val isRoot: Boolean
    }
    object QueryLike {

      final case class Ref(term: Term, param: Function.Param, tableSchema: Expr[TableRepr[?, ?]], isRoot: Boolean) extends QueryLike {
        def rowRepr: Expr[RowRepr[?]] = '{ $tableSchema.rowRepr }
      }

      final case class ProductField(term: Term, inner: QueryLike, field: String) extends QueryLike {
        override val param: Function.Param = inner.param
        override val tableSchema: Expr[TableRepr[?, ?]] = inner.tableSchema
        override val isRoot: Boolean = inner.isRoot
        def rowRepr: Expr[RowRepr[?]] = productSchemaField(term, field, inner.rowRepr)
      }

      final case class OptionGet(term: Term, inner: QueryLike) extends QueryLike {
        override val param: Function.Param = inner.param
        override val tableSchema: Expr[TableRepr[?, ?]] = inner.tableSchema
        override val isRoot: Boolean = inner.isRoot
        def rowRepr: Expr[RowRepr[?]] = optionSchemaGet(term, inner.rowRepr)
      }

    }

    sealed trait Binary extends QueryExpr

    final case class AndOr(term: Term, lhs: QueryExpr, op: BinOp.AndOr, rhs: QueryExpr) extends QueryExpr.Binary

    sealed trait Comp extends Binary {
      val lhs: Unary
      val op: BinOp.Comp
      val rhs: Unary
    }
    object Comp {
      final case class Case1(term: Term, lhs: QueryLike, op: BinOp.Comp, rhs: QueryLike) extends Comp
      final case class Case2(term: Term, lhs: QueryLike, op: BinOp.Comp, rhs: InputLike) extends Comp
      final case class Case3(term: Term, lhs: InputLike, op: BinOp.Comp, rhs: QueryLike) extends Comp
    }

    override def parse(expr: RawQueryExpr)(using ParseContext): ParseResult[QueryExpr] =
      expr match {
        case expr: RawQueryExpr.Unary =>
          Unary.parse(expr)
        case RawQueryExpr.Binary(term, lhs, op: BinOp.Comp, rhs) =>
          for {
            lhs <- lhs match {
              case lhs: RawQueryExpr.Unary => ParseResult.Success(lhs)
              case _                       => ParseResult.error(term, "can only compare unary operations")
            }
            rhs <- rhs match {
              case rhs: RawQueryExpr.Unary => ParseResult.Success(rhs)
              case _                       => ParseResult.error(term, "can only compare unary operations")
            }
            lhs <- Unary.parse(lhs)
            rhs <- Unary.parse(rhs)
            expr <- (lhs, rhs) match {
              case (lhs: QueryLike, rhs: QueryLike) => ParseResult.Success(Comp.Case1(term, lhs, op, rhs))
              case (lhs: QueryLike, rhs: InputLike) => ParseResult.Success(Comp.Case2(term, lhs, op, rhs))
              case (lhs: InputLike, rhs: QueryLike) => ParseResult.Success(Comp.Case3(term, lhs, op, rhs))
              case (_: InputLike, _: InputLike)     => ParseResult.error(term, "can not compare 2 inputs")
            }
          } yield expr
        case RawQueryExpr.Binary(term, lhs, op: BinOp.AndOr, rhs) =>
          for {
            lhs <- QueryExpr.parse(lhs)
            rhs <- QueryExpr.parse(rhs)
          } yield QueryExpr.AndOr(term, lhs, op, rhs)
      }

  }

  private sealed trait RawQueryExpr {

    val term: Term

    final def isBin: Boolean = this match
      case _: RawQueryExpr.Unary  => false
      case _: RawQueryExpr.Binary => true

    final def show: String = this match {
      case RawQueryExpr.Ref(_, ref)                                   => ref.show
      case RawQueryExpr.ProductField(_, ref, field, _)                => s"${ref.show}.${field.magentaFg}"
      case RawQueryExpr.OptionGet(_, ref)                             => s"${ref.show}.${"get".hexFg("#35A7FF")}"
      case bin: RawQueryExpr.Binary if bin.lhs.isBin || bin.rhs.isBin => s"(${bin.lhs.show}) ${bin.op.show} (${bin.rhs.show})"
      case bin: RawQueryExpr.Binary                                   => s"${bin.lhs.show} ${bin.op.show} ${bin.rhs.show}"
    }

  }
  private object RawQueryExpr extends Parser[(Term, RefMap), RawQueryExpr] {

    sealed trait Unary extends RawQueryExpr
    object Unary extends Parser[(Term, RefMap), RawQueryExpr.Unary] {

      override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[RawQueryExpr.Unary] =
        RawQueryExpr.parse(input).flatMap {
          case unary: Unary => ParseResult.Success(unary)
          case _: Binary    => ParseResult.error(input._1, "expected unary")
        }

    }

    final case class Ref(term: Term, ref: QueryReference) extends RawQueryExpr.Unary
    final case class ProductField(term: Term, ref: RawQueryExpr.Unary, field: String, access: Expr[Any] => Expr[Any]) extends RawQueryExpr.Unary
    final case class OptionGet(term: Term, ref: RawQueryExpr.Unary) extends RawQueryExpr.Unary

    final case class Binary(term: Term, lhs: RawQueryExpr, op: BinOp, rhs: RawQueryExpr) extends RawQueryExpr
    object Binary extends Parser[(Term, RefMap), RawQueryExpr.Binary] {

      override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[RawQueryExpr.Binary] =
        RawQueryExpr.parse(input).flatMap {
          case bin: Binary => ParseResult.Success(bin)
          case _: Unary    => ParseResult.error(input._1, "expected binary")
        }

    }

    private object parseProductField extends Parser[Select, (Term, String, Expr[Any] => Expr[Any])] {

      override def parse(term: Select)(using ParseContext): ParseResult[(Term, String, Expr[Any] => Expr[Any])] =
        term match {
          case Select(lhs, funct) =>
            type T
            given Type[T] = lhs.tpe.widen.asTyped

            // TODO (KR) : consider requiring an `extends Column.Product`
            for {
              gen <- k0.ProductGeneric.attemptOf[T] match {
                case Right(value) => ParseResult.Success(value)
                case Left(_)      => ParseResult.unknown(lhs, "no product generic")
              }
              field <- gen.fields.find(_.name == funct) match {
                case Some(field) => ParseResult.Success(field)
                case None        => ParseResult.error(term, s"not a product field: $funct")
              }
            } yield (lhs, funct, field.get.asInstanceOf[Expr[Any] => Expr[Any]])
        }

    }

    private object parseOptionGet extends Parser[Select, Term] {

      override def parse(term: Select)(using ParseContext): ParseResult[Term] = term match
        case Select(lhs, "get") if lhs.tpe <:< TypeRepr.of[Option[?]] => ParseResult.Success(lhs)
        case Select(lhs, funct) if lhs.tpe <:< TypeRepr.of[Option[?]] => ParseResult.error(term, s"invalid function call on Option: $funct")
        case _                                                        => ParseResult.unknown(term, "LHS is not an Option")

    }

    override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[RawQueryExpr] = {
      val (rootTerm, refs) = input
      rootTerm match {
        case ident: Ident =>
          refs.get(ident.symbol) match {
            case Some(ref) => ParseResult.Success(Ref(rootTerm, ref))
            case None      => ParseResult.error(ident, "not an input or query param")
          }
        case parseOptionGet.optional(res) =>
          for {
            term <- res
            expr <- Unary.parse((term, refs))
          } yield OptionGet(rootTerm, expr)
        case parseProductField.optional(res) =>
          for {
            (term, field, access) <- res
            expr <- Unary.parse((term, refs))
          } yield ProductField(rootTerm, expr, field, access)
        case singleApply(select @ Select(lhs, op), rhs) =>
          for {
            op <- op match
              case BinOp.Scala(op) => ParseResult.Success(op)
              case _               => ParseResult.error(select, s"invalid binary operator: $op")
            lhs <- RawQueryExpr.parse((lhs, refs))
            rhs <- RawQueryExpr.parse((rhs, refs))
          } yield Binary(rootTerm, lhs, op, rhs)
        case singleApply(singleApply(ident @ Ident(op), lhs), rhs) =>
          for {
            op <- op match
              case BinOp.Scala(op) => ParseResult.Success(op)
              case _               => ParseResult.error(ident, s"invalid binary operator: $op")
            lhs <- RawQueryExpr.parse((lhs, refs))
            rhs <- RawQueryExpr.parse((rhs, refs))
          } yield Binary(rootTerm, lhs, op, rhs)
        case _ =>
          ParseResult.unknown(rootTerm, "not a query expr?")
      }
    }

  }

  private final class RefMap private (map: Map[Symbol, QueryReference]) {
    def get(sym: Symbol): Option[QueryReference] = map.get(sym)
    def add(elems: (Symbol, QueryReference)*): RefMap = new RefMap(map ++ elems.toMap)
    def addList(elems: List[(Symbol, QueryReference)]): RefMap = new RefMap(map ++ elems.toMap)
  }
  private object RefMap {
    val empty: RefMap = new RefMap(Map.empty)
  }

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
    )(using ParseContext): ParseResult.Known[(List[Join], String, RefMap, Term)] =
      Join.parse((term, refs)) match
        case ParseResult.Success((join, funct, refs, term)) => joinLoop(join :: stack, funct, refs, term)
        case error: ParseResult.Error                       => error
        case _: ParseResult.Unknown                         => ParseResult.Success((stack.reverse, funct, refs, term))

    override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[Repr] =
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

      override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[(From, String, RefMap, Term)] =
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

      override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[(Join, String, RefMap, Term)] =
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

      override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[(Where, String, Term)] =
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

      override def parse(input: (Term, RefMap))(using ParseContext): ParseResult[Returning] =
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

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extractors
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object singleApply {

    def unapply(term: Term): Option[(Term, Term)] = term match
      case Apply(TypeApply(fun, _), arg :: Nil) => (fun, arg).some
      case Apply(fun, arg :: Nil)               => (fun, arg).some
      case _                                    => None

  }

  private object manyApply {

    def unapply(term: Term): Option[(Term, List[Term])] = term match
      case Apply(TypeApply(fun, _), args) => (fun, args).some
      case Apply(fun, args)               => (fun, args).some
      case _                              => None

  }

  private object tupleApply {

    def unapply(term: Term): Option[NonEmptyList[Term]] = term match
      case manyApply(Select(Ident(ident), "apply"), args) if ident.startsWith("Tuple") => args.toNonEmpty
      case _                                                                           => None

  }

  private object functionCall extends Parser[Term, (Term, Ident | Select, Function)] {

    override def parse(input: Term)(using ParseContext): ParseResult[(Term, Ident | Select, Function)] = input match
      case singleApply(singleApply(ident: Ident, lhs), Function.required(function)) => function.map((lhs, ident, _))
      case singleApply(select: Select, Function.required(function))                 => function.map((select.qualifier, select, _))
      case _                                                                        => ParseResult.unknown(input, "not a function call")

  }

  private object functionNames {

    extension (self: Ident | Select)
      private def getName: String = self match
        case Ident(name)     => name
        case Select(_, name) => name

    object mapOrFlatMap extends Parser[Ident | Select, String] {

      override def parse(input: Ident | Select)(using ParseContext): ParseResult[String] = input.getName match
        case name @ ("flatMap" | "map") => ParseResult.Success(name)
        case name                       => ParseResult.unknown(input, s"expected `map`/`flatMap`, got: $name")

    }

    object withFilter extends Parser[Ident | Select, String] {

      override def parse(input: Ident | Select)(using ParseContext): ParseResult[String] = input.getName match
        case name @ "withFilter" => ParseResult.Success(name)
        case name                => ParseResult.unknown(input, s"expected `withFilter`, got: $name")

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Function
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final case class Function(
      rootTree: Tree,
      params: List[Function.Param],
      body: Term,
  ) {

    def parseEmptyParams(using ParseContext): ParseResult.Known[Unit] = params match
      case Nil => ParseResult.Success(())
      case _   => ParseResult.error(body, s"expected single function param, but got ${params.size} - ${params.map(_.name).mkString(", ")}") // TODO (KR) : whole function pos

    def parseSingleParam(using ParseContext): ParseResult.Known[Function.Param] = params match
      case p :: Nil => ParseResult.Success(p)
      case _        => ParseResult.error(body, s"expected single function param, but got ${params.size} - ${params.map(_.name).mkString(", ")}") // TODO (KR) : whole function pos

    def showParams: String = Function.showParams(params)

  }
  private object Function extends Parser[Term, Function] {

    final case class Param(
        name: String,
        tpe: TypeRepr,
        tree: Tree,
        fromInput: Option[Expr[Any] => Expr[Any]],
    ) {

      def toIndentedString: IndentedString =
        IndentedString.section("Param:")(
          s"name: $name",
          s"type: ${tpe.show}",
        )

    }

    def showParams(params: List[Function.Param]): String =
      s"params(${params.map { p => s"\n  ${p.name}: ${p.tpe.show} <${p.tree.symbol.fullName}>," }.mkString}\n) "

    private def makeTupleAccess(size: Int, idx: Int): Expr[Any] => Expr[Any] = {
      val selectSym: Symbol = defn.TupleClass(size).fieldMember(s"_${idx + 1}")
      _.toTerm.select(selectSym).asExpr
    }

    private def parseDefDef(term: Term)(using ParseContext): ParseResult[(ValDef, Term)] =
      for {
        defDef <- term match {
          case Block((defDef: DefDef) :: Nil, Closure(_, _)) => ParseResult.Success(defDef)
          case _                                             => ParseResult.unknown(term, "not a block with a closure")
        }

        params <- defDef.paramss match {
          case head :: Nil => ParseResult.Success(head)
          case params      => ParseResult.error(defDef, s"Expected DefDef to have single param group, but had (${params.size})")
        }
        valDef <- params.params match {
          case Left(valDef :: Nil) => ParseResult.Success(valDef)
          case Left(valDefs)       => ParseResult.error(defDef, s"Expected DefDef to have single val def, but had (${valDefs.size})")
          case Right(_)            => ParseResult.error(defDef, "Expected val params, not type params")
        }

        rhs <- defDef.rhs match {
          case Some(rhs) => ParseResult.Success(rhs)
          case None      => ParseResult.error(defDef, "DefDef does not have RHS")
        }
      } yield (valDef, rhs)

    private def parseRHSCaseDef(caseDef: CaseDef)(using ParseContext): ParseResult[Function] =
      caseDef.pattern match {
        case Unapply(fun, Nil, patterns) =>
          for {
            _ <- fun match {
              case TypeApply(Select(Ident(name), "unapply"), _) if name.startsWith("Tuple") => ParseResult.Success(())
              case _                                                                        => ParseResult.error(fun, "unapply a non-tuple")
            }
            params <- patterns.zipWithIndex.traverse {
              case (bind @ Bind("_", _), _)                     => ParseResult.error(bind, "ignored tuple args currently not supported")
              case (bind @ Bind(name, ident @ Ident("_")), idx) => ParseResult.success(Param(name, ident.tpe, bind, makeTupleAccess(patterns.size, idx).some))
              case (pat, _)                                     => ParseResult.error(pat, "invalid case pattern")
            }
          } yield Function(caseDef, params, caseDef.rhs)
        case Ident("_") => ParseResult.Success(Function(caseDef, Nil, caseDef.rhs))
        case _          => ParseResult.error(caseDef, "unable to parse match case")
      }

    private def parseRHSMatch(valDef: ValDef, mat: Match)(using ParseContext): ParseResult[Function] =
      for {
        _ <- ParseResult.validate(mat.scrutinee.symbol == valDef.symbol)(mat, "function match doesnt reference input?")
        caseDef <- mat.cases match {
          case caseDef :: Nil => ParseResult.Success(caseDef)
          case cases          => ParseResult.error(mat, s"match has non-single case (${cases.size})")
        }
        function <- parseRHSCaseDef(caseDef)
      } yield function

    private def parseRHS(root: Term, valDef: ValDef, rhs: Term)(using ParseContext): ParseResult[Function] =
      rhs match {
        case mat: Match => parseRHSMatch(valDef, mat)
        case _          => ParseResult.Success(Function(root, Param(valDef.name, valDef.tpt.tpe, valDef, None) :: Nil, rhs))
      }

    override def parse(term: Term)(using ParseContext): ParseResult[Function] =
      for {
        (valDef, rhs) <- ParseContext.add("core function structure") { parseDefDef(term) }
        function <- ParseContext.add("function rhs") { parseRHS(term, valDef, rhs) }
      } yield function

  }

}
