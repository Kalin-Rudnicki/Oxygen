package oxygen.sql.generic.parsing

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*

private[generic] final class RefMap private (map: Map[Symbol, QueryReference]) {

  def allQueryRefs: Growable[QueryReference] = Growable.many(map.values)

  def add(elems: QueryReference*): RefMap = new RefMap(map ++ elems.map { qr => qr.param.sym -> qr }.toMap)
  def addAlias(from: Function.Param, to: Function.Param): RefMap = new RefMap(map ++ map.get(to.sym).map { (from.sym, _) }.iterator.toMap)
  def addList(elems: List[QueryReference]): RefMap = new RefMap(map ++ elems.map { qr => qr.param.sym -> qr }.toMap)

  def find(ident: Ident): Option[QueryReference] = map.get(ident.symbol)

  def get(ident: Ident)(using ParseContext): ParseResult[QueryReference] =
    find(ident) match {
      case Some(queryRef) => ParseResult.Success(queryRef)
      case None           => ParseResult.error(ident, "ident does not belong to an input/query param")
    }
  def getInput(ident: Ident)(using ParseContext): ParseResult[QueryReference.InputLike] =
    find(ident) match {
      case Some(queryRef: QueryReference.InputLike) => ParseResult.Success(queryRef)
      case Some(_: QueryReference.Query)            => ParseResult.error(ident, "ident belongs to a query param, but an input param is expected")
      case None                                     => ParseResult.error(ident, "ident does not belong to an input/query param")
    }
  def getQuery(ident: Ident)(using ParseContext): ParseResult[QueryReference.Query] =
    find(ident) match {
      case Some(queryRef: QueryReference.Query) => ParseResult.Success(queryRef)
      case Some(_: QueryReference.InputLike)    => ParseResult.error(ident, "ident belongs to an input param, but a query param is expected")
      case None                                 => ParseResult.error(ident, "ident does not belong to an input/query param")
    }

  def getRootQueryRef(errorPos: Tree)(using ParseContext): ParseResult[QueryReference.Query] =
    map.valuesIterator.collect { case ref: QueryReference.Query if ref.isRoot => ref }.toList match
      case ref :: Nil => ParseResult.Success(ref)
      case Nil        => ParseResult.error(errorPos, "no root param found?")
      case _          => ParseResult.error(errorPos, "many root params found?")

}
private object RefMap {
  val empty: RefMap = new RefMap(Map.empty)
}
