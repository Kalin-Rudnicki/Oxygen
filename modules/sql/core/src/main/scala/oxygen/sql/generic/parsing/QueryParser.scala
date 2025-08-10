package oxygen.sql.generic.parsing

import oxygen.core.typeclass.Zip3
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.query.dsl.{Q, T}
import oxygen.sql.schema.*
import scala.annotation.tailrec
import scala.quoted.*

private[generic] trait QueryParser[+A] {

  def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)]

}
private[generic] object QueryParser {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Parts
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Input(
      queryRef: QueryReference.InputLike,
  ) {

    def show: String =
      s"""
         |  [INPUT] ${queryRef.show}: ${queryRef.param.tpe.showAnsiCode}""".stripMargin

  }
  object Input extends QueryParser[Input] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Input, String, RefMap, Term)] = {
      for {
        (fc1, input) <- FunctionCall.parseTyped[T.InputLike](term, "function 1").parseLhsAndFunct {
          case ('{ Q.input.apply[a] }, fc1)            => fc1.funct.parseParam1.map { p1 => Input(QueryReference.InputParam(p1)) }
          case ('{ Q.input.const[a](${ expr }) }, fc1) => fc1.funct.parseParam1.map { p1 => Input(QueryReference.ConstInput(p1, expr.toTerm, TypeRepr.of[Any])) }
        }
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError

        newRefs = refs.add(input.queryRef)

      } yield (input, f1Name, newRefs, fc1.funct.body)

    }

  }

  final case class InsertQ(
      queryRef: Option[QueryReference.Query],
      tableRepr: Expr[TableRepr[?]],
      intoParam: Function.Param,
  ) {

    def rowRepr(using Quotes): Expr[RowRepr[?]] =
      '{ $tableRepr.rowRepr }

    def show: String =
      queryRef.fold("insert(???)")(queryRef => s"${queryRef.param.tpe.showAnsiCode} ${queryRef.show}")

  }
  object InsertQ extends QueryParser[InsertQ] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(InsertQ, String, RefMap, Term)] =
      for {
        (fc1, tableRepr) <- FunctionCall.parseTyped[T.Insert[?]](term, "function 1").parseLhs { case '{ Q.insert[a](using $tableRepr) } => ParseResult.Success(tableRepr) }
        (p1, p2) <- fc1.funct.parseParam2Opt
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError

        queryRef = p1.map { p1 => QueryReference.Query(p1, tableRepr, true) }
        newRefs = refs.add(queryRef.toList*)

      } yield (InsertQ(queryRef, tableRepr, p2), f1Name, newRefs, fc1.funct.body)

  }

  final case class SelectQ(
      queryRef: QueryReference.Query,
      tableRepr: Expr[TableRepr[?]],
  ) {

    def show: String =
      s"FROM ${queryRef.param.tpe.showAnsiCode} ${queryRef.show}"

  }
  object SelectQ extends QueryParser[SelectQ] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(SelectQ, String, RefMap, Term)] =
      for {
        (fc1, tableRepr) <- FunctionCall.parseTyped[T.Select[?]](term, "function 1").parseLhs { case '{ Q.select[a](using $tableRepr) } => ParseResult.Success(tableRepr) }
        p1 <- fc1.funct.parseParam1
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError

        queryRef = QueryReference.Query(p1, tableRepr, true)
        newRefs = refs.add(queryRef)

      } yield (SelectQ(queryRef, tableRepr), f1Name, newRefs, fc1.funct.body)

  }

  final case class UpdateQ(
      eitherQueryRef: Either[Function.RootParam.Ignored, QueryReference.Query],
      tableRepr: Expr[TableRepr[?]],
      setParam: Function.Param,
  ) {

    def queryRef: Option[QueryReference.Query] =
      eitherQueryRef.toOption

    def queryRefOrPlaceholder: QueryReference.Query =
      eitherQueryRef match {
        case Right(queryRef) => queryRef
        case Left(ignored)   => QueryReference.Query(Function.RootParam.Named(ignored.valDef), tableRepr, true)
      }

    def show: String =
      queryRef.fold("update(???)") { queryRef => s"${queryRef.param.tpe.showAnsiCode} ${queryRef.show}" }

  }
  object UpdateQ extends QueryParser[UpdateQ] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(UpdateQ, String, RefMap, Term)] =
      for {
        (fc1, tableRepr) <- FunctionCall.parseTyped[T.Update[?]](term, "function 1").parseLhs { case '{ Q.update[a](using $tableRepr) } => ParseResult.Success(tableRepr) }
        (p1, p2) <- fc1.funct.parseParam2Either
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError

        queryRef = p1.map { p1 => QueryReference.Query(p1, tableRepr, true) }
        update = UpdateQ(queryRef, tableRepr, p2)
        newRefs = refs.add(update.queryRefOrPlaceholder)

      } yield (update, f1Name, newRefs, fc1.funct.body)

  }

  final case class DeleteQ(
      queryRef: QueryReference.Query,
      tableRepr: Expr[TableRepr[?]],
  ) {

    def show: String =
      s"${queryRef.param.tpe.showAnsiCode} ${queryRef.show}"

  }
  object DeleteQ extends QueryParser[DeleteQ] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(DeleteQ, String, RefMap, Term)] =
      for {
        (fc1, tableRepr) <- FunctionCall.parseTyped[T.Delete[?]](term, "function 1").parseLhs { case '{ Q.delete[a](using $tableRepr) } => ParseResult.Success(tableRepr) }
        p1 <- fc1.funct.parseParam1
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError

        queryRef = QueryReference.Query(p1, tableRepr, true)
        newRefs = refs.add(queryRef)

      } yield (DeleteQ(queryRef, tableRepr), f1Name, newRefs, fc1.funct.body)

  }

  final case class Join(
      escapingParam: QueryReference.Query,
      onParam: QueryReference.Query,
      on: QueryExpr,
      tableRepr: Expr[TableRepr[?]],
  ) {

    def show(using Quotes): String =
      s"""
         |    Join ${onParam.param.tpe.showAnsiCode} ${onParam.show}
         |      ON ${on.show}""".stripMargin

    def queryRefs: Growable[QueryReference] =
      escapingParam +: onParam +: on.queryRefs

  }
  object Join extends QueryParser[Join] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Join, String, RefMap, Term)] =
      for {
        fc1 <- FunctionCall.parseTyped[T.Join[?]](term, "function 1").ignore
        p1 <- fc1.funct.parseParam1
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
        (fc2, tableRepr) <- FunctionCall
          .parseTyped[T.Partial.JoinLike](fc1.lhs, "function 2")
          .parseLhs {
            // TODO (KR) : left join
            case '{ oxygen.sql.query.dsl.Q.join[a](using $tableRepr) } => ParseResult.Success(tableRepr)
          }
          .unknownAsError
        p2 <- fc2.funct.parseParam1
        _ <- functionNames.withFilter.parse(fc2.nameRef).unknownAsError

        escapingParam = QueryReference.Query(p1, tableRepr, false)
        onParam = QueryReference.Query(p2, tableRepr, false)
        newRefs = refs.add(
          escapingParam,
          onParam,
        )

        onExpr <- RawQueryExpr.parse((fc2.funct.body.simplify, newRefs)).unknownAsError
        onExpr <- QueryExpr.parse(onExpr)

      } yield (Join(escapingParam, onParam, onExpr, tableRepr), f1Name, newRefs, fc1.funct.body)

  }

  final case class Where(
      where: QueryExpr,
  ) {

    def show(using Quotes): String =
      s"""
         |    WHERE ${where.show}""".stripMargin

  }
  object Where extends QueryParser[Where] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Where, String, RefMap, Term)] = {
      for {
        fc1 <- FunctionCall.parseTyped[T.Where](term, "function 1").ignore
        _ <- fc1.funct.parseEmptyParams
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
        fc2 <- FunctionCall.parseTyped[T.Partial.Where](fc1.lhs, "function 2").filterLhs { case '{ Q.where } => }.unknownAsError
        _ <- fc2.funct.parseEmptyParams
        _ <- functionNames.withFilter.parse(fc2.nameRef).unknownAsError

        whereExpr <- RawQueryExpr.parse((fc2.funct.body.simplify, refs)).unknownAsError
        whereExpr <- QueryExpr.parse(whereExpr).unknownAsError

      } yield (Where(whereExpr), f1Name, refs, fc1.funct.body)
    }

  }

  final case class Into(
      queryExpr: QueryExpr.InputLike.QueryRefIdent,
  )
  object Into extends QueryParser[Into] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Into, String, RefMap, Term)] =
      for {
        fc1 <- FunctionCall.parseTyped[T.InsertValues](term, "function 1").ignore
        _ <- fc1.funct.parseEmptyParams
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
        (_, valueIdent) <- fc1.lhs match { // TODO (KR) : validate `intoIdent`
          case Apply(Select(intoIdent: Ident, "apply"), (valueIdent: Ident) :: Nil) => ParseResult.Success((intoIdent, valueIdent))
          case _                                                                    => ParseResult.error(fc1.lhs, "does not look like `into(...)`")
        }

        queryRef <- refs.getInput(valueIdent)
        queryExpr = QueryExpr.InputLike.QueryRefIdent(valueIdent, queryRef)
      } yield (Into(queryExpr), f1Name, refs, fc1.funct.body)

  }

  final case class Set(
      parts: NonEmptyList[Set.SetPart],
  ) {

    def show(using Quotes): String =
      s"    SET ${parts.head.show}${parts.tail.map { p => s"\n        ${p.show}" }.mkString}"

  }
  object Set extends QueryParser[Set] {

    final case class SetPart(
        lhsExpr: QueryExpr.QueryLike,
        rhsExpr: QueryExpr.Unary,
    ) {

      def show(using Quotes): String = s"${lhsExpr.show} := ${rhsExpr.show}"

    }

    private def parseSetPart(term: Term, refs: RefMap, rootQueryRef: QueryReference.Query)(using ParseContext, Quotes): ParseResult[SetPart] =
      for {
        fun <- Function.parse(term).unknownAsError
        p1 <- fun.parseParam1
        (lhs, rhs) <- fun.body match {
          case Apply(Apply(TypeApply(Ident(":="), _ :: Nil), lhs :: Nil), rhs :: Nil) => ParseResult.Success((lhs, rhs))
          case _                                                                      => ParseResult.error(fun.body, "invalid set part")
        }

        _ <- ParseResult.validate(p1.tpe =:= rootQueryRef.param.tpe)(p1.tree, "set param does not match type of update table?")
        widenedLhsTpe = lhs.tpe.widen
        widenedRhsTpe = rhs.tpe.widen

        refsWithParam = refs.addAlias(p1, rootQueryRef.param)
        lhsExpr <- RawQueryExpr.Unary.parse((lhs, refsWithParam)).unknownAsError
        lhsExpr <- QueryExpr.Unary.parse(lhsExpr).unknownAsError
        lhsExpr <- lhsExpr match {
          case lhsExpr: QueryExpr.QueryLike if lhsExpr.param.sym == rootQueryRef.param.sym => ParseResult.Success(lhsExpr)
          case _                                                                           => ParseResult.error(lhsExpr.rootIdent, "root of set lhs is not the update table?")
        }

        rhsExpr <- RawQueryExpr.Unary.parse((rhs, refs)).unknownAsError
        rhsExpr <- QueryExpr.Unary.parse(rhsExpr).unknownAsError

        _ <- ParseResult.validate(widenedLhsTpe <:< widenedRhsTpe)(fun.body, s"set types do not match:  ${widenedLhsTpe.showAnsiCode} := ${widenedRhsTpe.showAnsiCode}")
      } yield SetPart(lhsExpr, rhsExpr)

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Set, String, RefMap, Term)] =
      for {
        fc1 <- FunctionCall.parseTyped[T.UpdateSet](term, "function 1").ignore
        _ <- fc1.funct.parseEmptyParams
        f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
        (_, setTerms) <- fc1.lhs match { // TODO (KR) : validate `setIdent`
          case Apply(Select(setIdent: Ident, "apply"), set0 :: Repeated.ignoreTyped(setN, _) :: Nil) => ParseResult.Success((setIdent, NonEmptyList(set0, setN)))
          case _                                                                                     => ParseResult.error(fc1.lhs, "does not look like `set(...)`")
        }

        rootQueryRef <- refs.getRootQueryRef(fc1.lhs)
        parts <- setTerms.traverse(parseSetPart(_, refs, rootQueryRef))
      } yield (Set(parts), f1Name, refs, fc1.funct.body)

  }

  final case class Returning(tree: Tree, exprs: List[QueryExpr]) {

    def showOpt(using Quotes): Option[String] = exprs match
      case Nil         => None
      case expr :: Nil => expr.show.some
      case exprs       => exprs.map(_.show).mkString(", ").some

  }
  object Returning extends Parser[(Term, RefMap), Returning] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[Returning] =
      for {
        (term, refs) <- ParseResult.Success(input)
        terms = term match
          case unitExpr()       => Nil
          case tupleApply(args) => args.toList
          case _                => term :: Nil
        exprs <- terms.traverse(t => RawQueryExpr.parse((t, refs)))
        exprs <- exprs.traverse(t => QueryExpr.parse(t))
      } yield Returning(term, exprs)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Aggregate
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  enum PartialQuery {

    case InsertQuery(
        insert: InsertQ,
        into: Into,
    )

    case SelectQuery(
        select: SelectQ,
        joins: List[Join],
        where: Option[Where],
    )

    case UpdateQuery(
        update: UpdateQ,
        joins: List[Join],
        where: Option[Where],
        set: Set,
    )

    case DeleteQuery(
        delete: DeleteQ,
        joins: List[Join],
        where: Option[Where],
    )

  }

  val partialInsert: QueryParser[PartialQuery.InsertQuery] =
    (
      InsertQ.withContext("Insert") >>>
        Into.withContext("Into")
    ).withContext("Insert Query").map { PartialQuery.InsertQuery.apply }

  val partialSelect: QueryParser[PartialQuery.SelectQuery] =
    (
      SelectQ.withContext("Select") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where")
    ).withContext("Select Query").map { PartialQuery.SelectQuery.apply }

  val partialUpdate: QueryParser[PartialQuery.UpdateQuery] =
    (
      UpdateQ.withContext("Update") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where") >>>
        Set.withContext("Set")
    ).withContext("Update Query").map { PartialQuery.UpdateQuery.apply }

  val partialDelete: QueryParser[PartialQuery.DeleteQuery] =
    (
      DeleteQ.withContext("Delete") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where")
    ).withContext("Delete Query").map { PartialQuery.DeleteQuery.apply }

  val partialQuery: QueryParser[PartialQuery] =
    partialInsert || partialSelect || partialUpdate || partialDelete

  val finished: Parser[Term, (List[Input], PartialQuery, Returning, RefMap)] =
    (Input.many.withContext("Input") >>> partialQuery).withReturning

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Ops
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: QueryParser[A]) {

    def >>>[B](that: QueryParser[B])(using zip: Zip[A, B]): QueryParser[zip.Out] =
      Then(self, that, zip)

    def ||[B >: A](that: QueryParser[B]): QueryParser[B] =
      Or(self, that)

    def withReturning(using zip: Zip3[A, Returning, RefMap]): Parser[Term, zip.Out] =
      new Parser[Term, zip.Out] {
        override def parse(input: Term)(using ParseContext, Quotes): ParseResult[zip.Out] =
          for {
            (aValue, aFunct, aRefs, aTerm) <- self.parse(input, RefMap.empty, "<init>")
            ret <- ParseContext.add("Returning") {
              ParseResult.validate(aFunct == "map")(aTerm, s"expected final `map`, not $aFunct\n\nparsed:\n$aValue").flatMap { _ =>
                Returning.parse((aTerm, aRefs))
              }
            }
          } yield zip.zip(aValue, ret, aRefs)
      }

    def map[B](f: A => B): QueryParser[B] =
      Mapped(self, f)

    def maybe: QueryParser[Option[A]] =
      Maybe(self)

    def many: QueryParser[List[A]] =
      Many(self)

    def withContext(ctx: String): QueryParser[A] =
      WithContext(self, ctx)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Combinators
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Maybe[A](inner: QueryParser[A]) extends QueryParser[Option[A]] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Option[A], String, RefMap, Term)] =
      inner.parse(term, refs, prevFunction) match {
        case ParseResult.Success(a, funct, refs, term) => ParseResult.Success(a.some, funct, refs, term)
        case _: ParseResult.Unknown                    => ParseResult.Success(None, prevFunction, refs, term)
        case error: ParseResult.Error                  => error
      }

  }

  final case class Many[A](inner: QueryParser[A]) extends QueryParser[List[A]] {

    @tailrec
    private def loop(
        acc: Growable[A],
        refs: RefMap,
        term: Term,
        prevFunction: String,
    )(using ParseContext, Quotes): ParseResult.Known[(List[A], String, RefMap, Term)] =
      inner.parse(term, refs, prevFunction) match
        case ParseResult.Success((a, funct, refs, term)) => loop(acc :+ a, refs, term, funct)
        case _: ParseResult.Unknown                      => ParseResult.Success((acc.to[List], prevFunction, refs, term))
        case error: ParseResult.Error                    => error

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(List[A], String, RefMap, Term)] =
      loop(Growable.empty, refs, term, prevFunction)

  }

  final case class Mapped[A, B](a: QueryParser[A], ab: A => B) extends QueryParser[B] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(B, String, RefMap, Term)] =
      a.parse(term, refs, prevFunction).map { case (a, funct, refs, term) => (ab(a), funct, refs, term) }

  }

  final case class Then[A, B, C](a: QueryParser[A], b: QueryParser[B], zip: Zip.Out[A, B, C]) extends QueryParser[C] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(C, String, RefMap, Term)] =
      for {
        (aValue, aFunct, aRefs, aTerm) <- a.parse(term, refs, prevFunction)
        (bValue, bFunct, bRefs, bTerm) <- b.parse(aTerm, aRefs, aFunct)
      } yield (zip.zip(aValue, bValue), bFunct, bRefs, bTerm)

  }

  final case class Or[A](a: QueryParser[A], b: QueryParser[A]) extends QueryParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
      a.parse(term, refs, prevFunction) match {
        case success @ ParseResult.Success(_) => success
        case unknown1: ParseResult.Unknown    =>
          b.parse(term, refs, prevFunction) match {
            case success @ ParseResult.Success(_) => success
            case unknown2: ParseResult.Unknown    => unknown1 ++ unknown2
            case error: ParseResult.Error         => error
          }
        case error: ParseResult.Error => error
      }

  }

  final case class WithContext[A](a: QueryParser[A], ctx: String) extends QueryParser[A] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(A, String, RefMap, Term)] =
      ParseContext.add(ctx) { a.parse(term, refs, prevFunction) }

  }

}
