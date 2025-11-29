package oxygen.sql.generic.model

import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.generation.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.*
import oxygen.sql.query.QueryContext.QueryType
import oxygen.sql.schema.*
import scala.quoted.*

private[sql] sealed trait ParsedQuery extends Product {

  val inputs: List[InputPart]
  val ret: ReturningPart
  val refs: RefMap

  def show(using Quotes): String

  protected final def showInputs: String =
    if (inputs.nonEmpty) inputs.map(_.show).mkString("inputs:", "", "\n")
    else "<no inputs>\n"

  protected final def showReturning(using Quotes): String =
    ret.showOpt.map { ret => s"\n    RETURNING $ret" }.mkString

  protected[ParsedQuery] def allQueryRefs: Growable[VariableReference]

  def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term]

}
private[sql] object ParsedQuery extends Parser[Term, ParsedQuery] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Insert
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait InsertQuery extends ParsedQuery
  object InsertQuery {

    final case class Basic(
        inputs: List[InputPart],
        insert: InsertPart.Basic,
        into: IntoPart,
        ret: ReturningPart,
        refs: RefMap,
    ) extends InsertQuery {

      override def show(using Quotes): String =
        s"""$showInputs
         |INSERT INTO ${insert.show}
         |    VALUES ${into.queryExpr.show}$showReturning
         |""".stripMargin

      override protected[ParsedQuery] def allQueryRefs: Growable[VariableReference] =
        Growable[Growable[VariableReference]](
          Growable.option(insert.mapQueryRef),
          into.queryExpr.queryRefs,
          Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
        ).flatten

      override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
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
            '{ ${ insert.tableRepr.expr }.rowRepr.columns.`a, b, c` },
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
          retA <- decoderBuilder.ret(ret, None)
          builtDecoder = retA.buildExpr

          // Combine
          expr = makeQuery(queryName, QueryContext.QueryType.Insert, insert.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
        } yield expr.toTerm

    }

    final case class FromSelect(
        inputs: List[InputPart],
        insert: InsertPart.FromSelect,
        select: ParsedQuery.SelectQuery,
        ret: ReturningPart,
        refs: RefMap,
    ) extends InsertQuery {

      override def show(using Quotes): String =
        s"""$showInputs
         |INSERT INTO ${insert.show}
         |${select.show}$showReturning
         |""".stripMargin

      override protected[ParsedQuery] def allQueryRefs: Growable[VariableReference] =
        Growable[Growable[VariableReference]](
          Growable.option(insert.mapQueryRef),
          select.allQueryRefs,
          Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
        ).flatten

      override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
        for {
          _ <- ParseResult.success(())

          // Input
          fragmentBuilder = FragmentBuilder(inputs)
          insertFrag <- fragmentBuilder.insert(insert)
          selectFrag <- select.makeFragment
          returningFrag <- fragmentBuilder.ret(ret, "              ")
          frag = GeneratedFragment.of(
            insertFrag,
            "\n    (",
            '{ ${ insert.tableRepr.expr }.rowRepr.columns.`a, b, c` },
            ")\n",
            selectFrag,
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
          retA <- decoderBuilder.ret(ret, None)
          builtDecoder = retA.buildExpr

          // Combine
          expr = makeQuery(queryName, QueryContext.QueryType.Insert, insert.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
        } yield expr.toTerm

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Select
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class SelectQuery(
      inputs: List[InputPart],
      select: SelectPart,
      joins: List[JoinPart],
      where: Option[WherePart],
      orderBy: Option[OrderByPart],
      limit: Option[LimitPart],
      offset: Option[OffsetPart],
      ret: ReturningPart,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |SELECT ${ret.showOpt.getOrElse("<ERROR>")}
         |    ${select.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}${orderBy.map(_.show).mkString}${limit.map(_.show).mkString}${offset.map(_.show).mkString}
         |""".stripMargin

    override protected[ParsedQuery] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        select match {
          case select: SelectPart.FromTable    => Growable(select.mapQueryRef)
          case select: SelectPart.FromSubQuery => Growable.many(select.mapQueryRefs)
        },
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.option(orderBy).flatMap(p => Growable.many(p.orderByExprs).map(_.queryExpr.queryRef)),
        Growable.option(limit).flatMap(_.limitQueryExpr.queryRefs),
        Growable.option(offset).flatMap(_.offsetQueryExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    def makeFragment(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
      val fragmentBuilder = FragmentBuilder(inputs)
      for {
        returningFrag <- fragmentBuilder.requiredRet(ret, "       ")
        selectFrag <- select match
          case select: SelectPart.FromTable    => fragmentBuilder.select(select)
          case select: SelectPart.FromSubQuery => fragmentBuilder.select(select)
        joinFrag <- joins.traverse(fragmentBuilder.join).map(GeneratedFragment.flatten(_))
        whereFrag <- where.traverse(fragmentBuilder.where).map(GeneratedFragment.option)
        orderByFrag <- orderBy.traverse(fragmentBuilder.orderBy).map(GeneratedFragment.option)
        limitFrag <- limit.traverse(fragmentBuilder.limit).map(GeneratedFragment.option)
        offsetFrag <- offset.traverse(fragmentBuilder.offset).map(GeneratedFragment.option)
      } yield GeneratedFragment.of(
        "SELECT ",
        returningFrag,
        selectFrag,
        joinFrag,
        whereFrag,
        orderByFrag,
        limitFrag,
        offsetFrag,
      )
    }

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
      for {
        frag <- makeFragment
        (builtSql, builtEncoder) = frag.buildExpr

        // Output
        decoderBuilder = new DecoderBuilder
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr
        _ <- ParseResult.validate(builtDecoder.nonEmpty)(ret.fullTree, "expected non-empty return")

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Select, select.optTableRepr, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }
  object SelectQuery extends Parser[Term, SelectQuery] {

    override def parse(input: Term)(using ParseContext, Quotes): ParseResult[SelectQuery] =
      PartialQuery.SelectQuery.fullParser.parse(input).map { case FullQueryResult(inputs, PartialQuery.SelectQuery(select, joins, where, orderBy, limit, offset), ret, refs) =>
        ParsedQuery.SelectQuery(inputs, select, joins, where, orderBy, limit, offset, ret, refs)
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Update
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class UpdateQuery(
      inputs: List[InputPart],
      update: UpdatePart,
      joins: List[JoinPart],
      where: Option[WherePart],
      set: SetPart,
      ret: ReturningPart,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |UPDATE ${update.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}
         |${set.show}$showReturning
         |""".stripMargin

    override protected[ParsedQuery] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        Growable(update.queryRefOrPlaceholder),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.many(set.setExprs.toList).flatMap(p => p.fieldToSetExpr.queryRef +: p.setValueExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
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
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Update, update.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Delete
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class DeleteQuery(
      inputs: List[InputPart],
      delete: DeletePart,
      joins: List[JoinPart],
      where: Option[WherePart], // TODO (KR) : make this required?
      ret: ReturningPart,
      refs: RefMap,
  ) extends ParsedQuery {

    override def show(using Quotes): String =
      s"""$showInputs
         |DELETE FROM ${delete.show}${joins.map(_.show).mkString}${where.map(_.show).mkString}$showReturning
         |""".stripMargin

    override protected[ParsedQuery] def allQueryRefs: Growable[VariableReference] =
      Growable[Growable[VariableReference]](
        Growable(delete.mapQueryRef),
        Growable.many(joins).flatMap(_.queryRefs),
        Growable.option(where).flatMap(_.filterExpr.queryRefs),
        Growable.many(ret.returningExprs).flatMap(_.expr.queryRefs),
      ).flatten

    override def toTerm(queryName: Expr[String], debug: Boolean)(using ParseContext, GenerationContext, Quotes): ParseResult[Term] =
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
        retA <- decoderBuilder.ret(ret, None)
        builtDecoder = retA.buildExpr

        // Combine
        expr = makeQuery(queryName, QueryContext.QueryType.Delete, delete.tableRepr.some, debug)(builtSql, builtEncoder, builtDecoder)
      } yield expr.toTerm

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Other
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def parse(input: Term)(using ParseContext, Quotes): ParseResult[ParsedQuery] =
    PartialQuery.fullParser
      .parse(input)
      .map {
        case FullQueryResult(inputs, PartialQuery.InsertQuery.Basic(insert, into), ret, refs) =>
          ParsedQuery.InsertQuery.Basic(inputs, insert, into, ret, refs)
        case FullQueryResult(inputs, PartialQuery.InsertQuery.FromSelect(insert, s, into), ret, refs) =>
          val fullSelect = ParsedQuery.SelectQuery(inputs, s.select, s.joins, s.where, s.orderBy, s.limit, s.offset, into.toReturning, refs)
          ParsedQuery.InsertQuery.FromSelect(inputs, insert, fullSelect, ret, refs)
        case FullQueryResult(inputs, PartialQuery.SelectQuery(select, joins, where, orderBy, limit, offset), ret, refs) =>
          ParsedQuery.SelectQuery(inputs, select, joins, where, orderBy, limit, offset, ret, refs)
        case FullQueryResult(inputs, PartialQuery.UpdateQuery(update, joins, where, set), ret, refs) =>
          ParsedQuery.UpdateQuery(inputs, update, joins, where, set, ret, refs)
        case FullQueryResult(inputs, PartialQuery.DeleteQuery(delete, joins, where), ret, refs) =>
          ParsedQuery.DeleteQuery(inputs, delete, joins, where, ret, refs)
      }
      .map { parsed =>
        val specifiedQueryRefs: Set[VariableReference.InputLike] = parsed.refs.allQueryRefs.collect { case ref: VariableReference.InputLike => ref }.to[Set]
        val usedQueryRefs: Set[VariableReference.InputLike] = parsed.allQueryRefs.collect { case ref: VariableReference.InputLike => ref }.to[Set]
        val unusedQueryRefs: Set[VariableReference.InputLike] = specifiedQueryRefs &~ usedQueryRefs
        unusedQueryRefs.foreach { ref => report.warning("unused query input param", ref.param.tree.pos) }

        parsed
      }

  def compile(queryName: Expr[String], input: Term, debug: Boolean)(using Quotes): (ParsedQuery, Term) =
    ParseContext.root("compile") {
      for {
        parsed <- ParseContext.add("parse") { ParsedQuery.parse(input) }
        res <- ParseContext.add(s"${parsed.productPrefix}.toTerm") { GenerationContext.root { parsed.toTerm(queryName, debug) } }
      } yield (parsed, res)
    }

  private given ToExprT[QueryContext.QueryType] = ToExprT.derived[QueryContext.QueryType]

  private def makeQuery(
      queryName: Expr[String],
      queryType: QueryContext.QueryType,
      mainTable: Option[TypeclassExpr.TableRepr],
      @scala.annotation.unused debug: Boolean,
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
          mainTable = ${
            mainTable match {
              case Some(mainTable) => '{ ${ mainTable.expr }.some }
              case None            => '{ None }
            }
          },
        )
      }

    (encoder.hasNonConstParams, encoder.hasParams, decoder.nonEmpty) match {
      case (true, _, true) => // QueryIO - non const inputs
        '{
          new QueryIO.Simple[I, O](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[I]] },
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (false, true, true) => // QueryO - with const inputs
        '{
          new QueryO.Simple[O](
            ctx = $ctx,
            encoder = ${ encoder.expr.asExprOf[InputEncoder[Any]] }.some,
            decoder = ${ decoder.expr.asExprOf[ResultDecoder[O]] },
          )
        }
      case (false, false, true) => // QueryO - no inputs
        '{
          new QueryO.Simple[O](
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
