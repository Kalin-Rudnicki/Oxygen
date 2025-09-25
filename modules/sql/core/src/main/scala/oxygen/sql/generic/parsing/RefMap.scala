package oxygen.sql.generic.parsing

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*

private[generic] final class RefMap private (map: Map[Symbol, QueryParam]) {

  def allQueryRefs: Growable[QueryParam] = Growable.many(map.values)

  def add(elems: QueryParam*): RefMap = new RefMap(map ++ elems.map { qr => qr.param.sym -> qr }.toMap)
  def addAlias(from: Function.NamedParam, to: Function.NamedParam): RefMap = new RefMap(map ++ map.get(to.sym).map { (from.sym, _) }.iterator.toMap)
  def addList(elems: List[QueryParam]): RefMap = new RefMap(map ++ elems.map { qr => qr.param.sym -> qr }.toMap)

  def find(ident: Ident): Option[QueryParam] = map.get(ident.symbol)

  def get(ident: Ident)(using ParseContext): ParseResult[QueryParam] =
    find(ident) match {
      case Some(queryRef) => ParseResult.Success(queryRef)
      case None           => ParseResult.error(ident, "ident does not belong to an input/query param")
    }
  def getInput(ident: Ident)(using ParseContext): ParseResult[QueryParam.InputLike] =
    find(ident) match {
      case Some(queryRef: QueryParam.InputLike) => ParseResult.Success(queryRef)
      case Some(_: QueryParam.Query)            => ParseResult.error(ident, "ident belongs to a query param, but an input param is expected")
      case None                                 => ParseResult.error(ident, "ident does not belong to an input/query param")
    }
  def getQuery(ident: Ident)(using ParseContext): ParseResult[QueryParam.Query] =
    find(ident) match {
      case Some(queryRef: QueryParam.Query) => ParseResult.Success(queryRef)
      case Some(_: QueryParam.InputLike)    => ParseResult.error(ident, "ident belongs to an input param, but a query param is expected")
      case None                             => ParseResult.error(ident, "ident does not belong to an input/query param")
    }

  def getRootQueryRef(errorPos: Tree)(using ParseContext): ParseResult[QueryParam.Query] =
    map.valuesIterator.collect { case ref: QueryParam.Query if ref.isRoot => ref }.toList match
      case ref :: Nil => ParseResult.Success(ref)
      case Nil        => ParseResult.error(errorPos, "no root param found?")
      case _          => ParseResult.error(errorPos, "many root params found?")

}
private object RefMap {
  val empty: RefMap = new RefMap(Map.empty)
}
