package oxygen.sql.generic

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
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

  final case class Input(queryRef: QueryReference.InputLike)
  object Input extends QueryParser[Input] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Input, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        p1 <- f1Function.parseParam1
        input <- f1Lhs.parseExpr[T.InputLike] {
          case '{ Q.input.apply[a] }            => ParseResult.Success(Input(QueryReference.InputParam(p1)))
          case '{ Q.input.const[a](${ expr }) } => ParseResult.Success(Input(QueryReference.ConstInput(p1, expr.toTerm)))
        }
        f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError

        newRefs = refs.add(p1.sym -> QueryReference.InputParam(p1))

      } yield (input, f1Name, newRefs, f1Function.body)

  }

  final case class Insert(
  )
  object Insert extends QueryParser[Insert] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Insert, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, _, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        _ <- f1Lhs.parseExpr[T.Partial.InsertValues[?]] { case '{ Q.select[a](using $schema) } => ParseResult.Success(schema) }
        _ = f1Function.params.foreach { p => report.info(p.tree.toIndentedString.toStringColorized, p.tree.pos) }
        _ <- ParseResult.error(term, "WTF TODO")
      } yield ???

  }

  final case class Select(
      param: Function.Param,
      schema: Expr[TableRepr[?]],
  )
  object Select extends QueryParser[Select] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Select, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        schema <- f1Lhs.parseExpr[T.Select[?]] { case '{ Q.select[a](using $schema) } => ParseResult.Success(schema) }
        p1 <- f1Function.parseParam1
        f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError

        newRefs = refs.add(p1.tree.symbol -> QueryReference.Query(p1, schema, true))

      } yield (Select(p1, schema), f1Name, newRefs, f1Function.body)

  }

  final case class Update()
  object Update extends QueryParser[Update] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Update, String, RefMap, Term)] =
      ParseResult.unknown(term, "TODO")

  }

  final case class Delete()
  object Delete extends QueryParser[Delete] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Delete, String, RefMap, Term)] =
      ParseResult.unknown(term, "TODO")

  }

  final case class Join(
      escapingParam: Function.Param,
      onParam: Function.Param,
      on: QueryExpr,
      schema: Expr[TableRepr[?]],
  )
  object Join extends QueryParser[Join] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Join, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        FunctionCall(f2Lhs, f2Name, f2Function) <- ParseContext.add("function 2") { FunctionCall.parse(f1Lhs) }
        schema <- f2Lhs.parseExpr[T.Partial.JoinLike] {
          // TODO (KR) : left join
          case '{ oxygen.sql.query.dsl.Q.join[a](using $schema) } => ParseResult.Success(schema)
        }
        p1 <- f1Function.parseParam1
        p2 <- f2Function.parseParam1
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

  final case class Where(
      where: QueryExpr,
  )
  object Where extends QueryParser[Where] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Where, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        FunctionCall(f2Lhs, f2Name, f2Function) <- ParseContext.add("function 2") { FunctionCall.parse(f1Lhs) }
        _ <- f2Lhs.parseExpr[T.Partial.Where] { case '{ Q.where } => ParseResult.Success(()) }
        _ <- f1Function.parseEmptyParams
        _ <- f2Function.parseEmptyParams
        f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError
        _ <- functionNames.withFilter.parse(f2Name).unknownAsError

        whereExpr <- RawQueryExpr.parse((f2Function.body.simplify, refs)).unknownAsError
        whereExpr <- QueryExpr.parse(whereExpr)

      } yield (Where(whereExpr), f1Name, refs, f1Function.body)

  }

  final case class Into()
  object Into extends QueryParser[Into] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Into, String, RefMap, Term)] =
      ParseResult.unknown(term, "TODO")

  }

  final case class Set()
  object Set extends QueryParser[Set] {

    override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Set, String, RefMap, Term)] =
      ParseResult.unknown(term, "TODO")

  }

  final case class Returning(tree: Tree, exprs: List[QueryExpr])
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
        insert: Insert,
        into: Into,
    )

    case SelectQuery(
        select: Select,
        joins: List[Join],
        where: Option[Where],
    )

    case UpdateQuery(
        update: Update,
        joins: List[Join],
        where: Option[Where],
        set: Set,
    )

    case DeleteQuery(
        delete: Delete,
        joins: List[Join],
        where: Option[Where],
    )

  }

  val partialInsert: QueryParser[PartialQuery.InsertQuery] =
    (
      Insert.withContext("Insert") >>>
        Into.withContext("Into")
    ).withContext("Insert Query").map { PartialQuery.InsertQuery.apply }

  val partialSelect: QueryParser[PartialQuery.SelectQuery] =
    (
      Select.withContext("Select") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where")
    ).withContext("Select Query").map { PartialQuery.SelectQuery.apply }

  val partialUpdate: QueryParser[PartialQuery.UpdateQuery] =
    (
      Update.withContext("Update") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where") >>>
        Set.withContext("Set")
    ).withContext("Update Query").map { PartialQuery.UpdateQuery.apply }

  val partialDelete: QueryParser[PartialQuery.DeleteQuery] =
    (
      Delete.withContext("Delete") >>>
        Join.many.withContext("Join") >>>
        Where.maybe.withContext("Where")
    ).withContext("Delete Query").map { PartialQuery.DeleteQuery.apply }

  val partialQuery: QueryParser[PartialQuery] =
    partialInsert || partialSelect || partialUpdate || partialDelete

  val finished: Parser[Term, (List[Input], PartialQuery, Returning)] =
    (Input.many.withContext("Input") >>> partialQuery).withReturning

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Ops
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [A](self: QueryParser[A]) {

    def >>>[B](that: QueryParser[B])(using zip: Zip[A, B]): QueryParser[zip.Out] =
      Then(self, that, zip)

    def ||[B >: A](that: QueryParser[B]): QueryParser[B] =
      Or(self, that)

    def withReturning(using zip: Zip[A, Returning]): Parser[Term, zip.Out] =
      new Parser[Term, zip.Out] {
        override def parse(input: Term)(using ParseContext, Quotes): ParseResult[zip.Out] =
          for {
            (aValue, aFunct, aRefs, aTerm) <- self.parse(input, RefMap.empty, "<init>")
            ret <- ParseContext.add("Returning") {
              ParseResult.validate(aFunct == "map")(aTerm, s"expected final `map`, not $aFunct").flatMap { _ =>
                Returning.parse((aTerm, aRefs))
              }
            }
          } yield zip.zip(aValue, ret)
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
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final class PartiallyAppliedParse[A: Type](self: Term) {

    def apply[B](pf: PartialFunction[Expr[A], ParseResult[B]])(using ParseContext, Quotes): ParseResult[B] =
      if (self.tpe <:< TypeRepr.of[A])
        pf.applyOrElse(
          self.asExprOf[A],
          _ => ParseResult.error(self, s"invalid ${TypeRepr.of[A].showCode}"),
        )
      else
        ParseResult.unknown(self, s"not a ${TypeRepr.of[A].showCode} (actual = ${self.tpe.showCode})")

  }

  extension (self: Term)
    private def parseExpr[A: Type]: PartiallyAppliedParse[A] =
      PartiallyAppliedParse[A](self)

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
