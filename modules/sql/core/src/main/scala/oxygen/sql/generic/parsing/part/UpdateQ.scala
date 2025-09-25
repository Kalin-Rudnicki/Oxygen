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
