package oxygen.sql.generic.parsing.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import oxygen.sql.schema.*
import scala.quoted.*

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
