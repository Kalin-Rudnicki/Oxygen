package oxygen.sql.generic.model

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.*
import oxygen.sql.query.QueryContext.QueryType
import oxygen.sql.schema.*
import scala.quoted.*

private[sql] sealed trait ParsedQuery extends Product {

  val inputs: List[QueryParser.Input]
  val ret: QueryParser.Returning
  val refs: RefMap

  def show(using Quotes): String

  protected final def showInputs: String =
    if (inputs.nonEmpty) inputs.map(_.show).mkString("inputs:", "", "\n")
    else "<no inputs>\n"

  protected final def showReturning(using Quotes): String =
    ret.showOpt.map { ret => s"\n    RETURNING $ret" }.mkString

  protected def allQueryRefs: Growable[QueryReference]

  def toTerm(queryName: Expr[String])(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
private[sql] object ParsedQuery extends Parser[Term, ParsedQuery] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Insert
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class InsertQuery(
      inputs: List[QueryParser.Input],
      insert: QueryParser.InsertQ,
      into: QueryParser.Into,
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |INSERT INTO ${insert.show}
         |    VALUES ${into.queryExpr.show}$showReturning
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable.option(insert.queryRef),
        Growable(into.queryExpr.queryRef),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String])(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        insertFrag <- fragmentBuilder.insert(insert)
        valuesFrag <- fragmentBuilder.values(insert, into)
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          insertFrag,
          "\n    (",
          '{ ${ insert.tableRepr }.rowRepr.columns.`a, b, c` },
          ")",
          valuesFrag,
          GeneratedFragment.option(
            returningFrag.map { ret =>
              GeneratedFragment.of(
                "\n    RETURNING ",
                ret,
              )
            },
          ),
        )
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Insert, insert.tableRepr)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Select
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class SelectQuery(
      inputs: List[QueryParser.Input],
      select: QueryParser.SelectQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |SELECT ${ret.showOpt.getOrElse("<ERROR>")}
         |    ${select.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(select.queryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.where.queryRefs),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String])(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        returningFrag <- fragmentBuilder.requiredRet(ret, "       ")
        selectFrag <- fragmentBuilder.select(select)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        frag = GeneratedFragment.of(
          "SELECT ",
          returningFrag,
          selectFrag,
          joinFrag,
          whereFrag,
        )
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret)
        builtDecoder = retA.buildExpr
        _ <- ParseResult.validate(builtDecoder.nonEmpty)(ret.tree, "expected non-empty return")

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Select, select.tableRepr)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Update
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class UpdateQuery(
      inputs: List[QueryParser.Input],
      update: QueryParser.UpdateQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where],
      set: QueryParser.Set,
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |UPDATE ${update.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |${set.show}$showReturning
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(update.queryRefOrPlaceholder),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.where.queryRefs),
        Growable.many(set.parts.toList).flatMap(p => Growable(p.lhsExpr.queryRef, p.rhsExpr.queryRef)),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String])(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        updateFrag <- fragmentBuilder.update(update)
        setFrag <- fragmentBuilder.set(set)
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          updateFrag,
          setFrag,
          whereFrag,
          GeneratedFragment.option(
            returningFrag.map { ret =>
              GeneratedFragment.of(
                "\n    RETURNING ",
                ret,
              )
            },
          ),
        )
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Update, update.tableRepr)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Delete
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class DeleteQuery(
      inputs: List[QueryParser.Input],
      delete: QueryParser.DeleteQ,
      joins: List[QueryParser.Join],
      where: Option[QueryParser.Where], // TODO (KR) : make this required?
      ret: QueryParser.Returning,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |DELETE FROM ${delete.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}$showReturning
         |""".stripMargin

    override protected def allQueryRefs: Growable[QueryReference] =
      Growable[Growable[QueryReference]](
        Growable(delete.queryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.where.queryRefs),
        Growable.many(ret.exprs).flatMap(_.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String])(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        _ <- ParseResult.success(())

        // Input
        fragmentBuilder = FragmentBuilder(inputs)
        deleteFrag <- fragmentBuilder.delete(delete)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        returningFrag <- fragmentBuilder.ret(ret, "              ")
        frag = GeneratedFragment.of(
          deleteFrag,
          joinFrag,
          whereFrag,
          GeneratedFragment.option(
            returningFrag.map { ret =>
              GeneratedFragment.of(
                "\n    RETURNING ",
                ret,
              )
            },
          ),
        )
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Delete, delete.tableRepr)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[ParsedQuery] =
    QueryParser.finished
      .parse(input)
      .map {
        case (inputs, QueryParser.PartialQuery.InsertQuery(insert, into), ret, refs) =>
          ParsedQuery.InsertQuery(inputs, insert, into, ret, refs)
        case (inputs, QueryParser.PartialQuery.SelectQuery(select, joins, where), ret, refs) =>
          ParsedQuery.SelectQuery(inputs, select, joins, where, ret, refs)
        case (inputs, QueryParser.PartialQuery.UpdateQuery(update, joins, where, set), ret, refs) =>
          ParsedQuery.UpdateQuery(inputs, update, joins, where, set, ret, refs)
        case (inputs, QueryParser.PartialQuery.DeleteQuery(delete, joins, where), ret, refs) =>
          ParsedQuery.DeleteQuery(inputs, delete, joins, where, ret, refs)
      }
      .map { parsed =>
        val specifiedQueryRefs: Set[QueryReference.InputLike] = parsed.refs.allQueryRefs.collect { case ref: QueryReference.InputLike => ref }.to[Set]
        val usedQueryRefs: Set[QueryReference.InputLike] = parsed.allQueryRefs.collect { case ref: QueryReference.InputLike => ref }.to[Set]
        val unusedQueryRefs: Set[QueryReference.InputLike] = specifiedQueryRefs &~ usedQueryRefs
        unusedQueryRefs.foreach { ref => report.warning("unused query input param", ref.param.tree.pos) }
        parsed
      }

  def compile(queryName: Expr[String], input: Term)(using Quotes): (ParsedQuery, Term) =
    ParseContext.root("compile") {
      for {
        parsed <- ParseContext.add("parse") { ParsedQuery.parse(input) }
        res <- ParseContext.add(s"${parsed.productPrefix}.toTerm") { GenerationContext.root { parsed.toTerm(queryName) } }
      } yield (parsed, res)
    }

  private given ToExpr[QueryContext.QueryType] =
    new ToExpr[QueryContext.QueryType] {
      override def apply(x: QueryContext.QueryType)(using Quotes): Expr[QueryContext.QueryType] = x match
        case QueryContext.QueryType.Select      => '{ QueryContext.QueryType.Select }
        case QueryContext.QueryType.Insert      => '{ QueryContext.QueryType.Insert }
        case QueryContext.QueryType.Update      => '{ QueryContext.QueryType.Update }
        case QueryContext.QueryType.Delete      => '{ QueryContext.QueryType.Delete }
        case QueryContext.QueryType.Transaction => '{ QueryContext.QueryType.Transaction }
        case QueryContext.QueryType.Truncate    => '{ QueryContext.QueryType.Truncate }
        case QueryContext.QueryType.Migrate     => '{ QueryContext.QueryType.Migrate }
    }

  private def makeQuery(
      queryName: Expr[String],
      queryType: QueryContext.QueryType,
      mainTable: Expr[TableRepr[?]],
  )(
      sql: GeneratedSql.Built,
      encoder: GeneratedInputEncoder.Built,
      decoder: GeneratedResultDecoder.Built,
  )(using Quotes): Expr[QueryLike] = {
    type I
    type O
    given Type[I] = encoder.tpe.asTypeOf
    given Type[O] = decoder.tpe.asTypeOf

    val ctx: Expr[QueryContext] =
      '{
        QueryContext(
          queryName = $queryName,
          sql = ${ sql.sql },
          queryType = ${ Expr(queryType) },
          mainTable = $mainTable.some,
        )
      }

    (encoder.hasNonConstParams, encoder.hasParams, decoder.nonEmpty) match {
      case (true, _, true) => // QueryIO - non const inputs
        '{
          new QueryIO[I, O](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[I]] },
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (false, true, true) => // QueryO - with const inputs
        '{
          new QueryO[O](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[Any]] }.some,
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (false, false, true) => // QueryO - no inputs
        '{
          new QueryO[O](
            ctx = $ctx,
            encoder = None,
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (true, _, false) => // QueryI - non const inputs
        '{
          new QueryI[I](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[I]] },
          )
        }
      case (false, true, false) => // Query - with const inputs
        '{
          new Query(
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[Any]] }.some,
          )
        }
      case (false, false, false) => // Query - no inputs
        '{
          new Query(
            ctx = $ctx,
            encoder = None,
          )
        }
    }
  }

}
