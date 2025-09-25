package oxygen.sql.generic.parsing.part


import oxygen.core.typeclass.Zip3
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import oxygen.sql.schema.*
import scala.annotation.tailrec
import scala.quoted.*

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
