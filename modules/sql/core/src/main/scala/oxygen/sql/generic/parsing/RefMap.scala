package oxygen.sql.generic.parsing

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*

private[generic] final class RefMap private (map: Map[Symbol, VariableReference]) {

  def allQueryRefs: Growable[VariableReference] = Growable.many(map.values)

  def add(elems: VariableReference*): RefMap = new RefMap(map ++ elems.map { qr => qr.internalParam.sym -> qr }.toMap)
  def addAlias(from: Function.NamedParam, to: Function.NamedParam): RefMap = new RefMap(map ++ map.get(to.sym).map { (from.sym, _) }.iterator.toMap)
  def addList(elems: List[VariableReference]): RefMap = new RefMap(map ++ elems.map { qr => qr.internalParam.sym -> qr }.toMap)

  def find(ident: Ident): Option[VariableReference] = map.get(ident.symbol)

  def get(ident: Ident)(using ParseContext): ParseResult[VariableReference] =
    find(ident) match {
      case Some(queryRef) => ParseResult.Success(queryRef)
      case None           => ParseResult.error(ident, "ident does not belong to an input/query param")
    }
  def getInput(ident: Ident)(using ParseContext): ParseResult[VariableReference.InputLike] =
    find(ident) match {
      case Some(queryRef: VariableReference.InputLike) => ParseResult.Success(queryRef)
      case Some(_: VariableReference.QueryLike)        => ParseResult.error(ident, "ident belongs to a query param, but an input param is expected")
      case None                                        => ParseResult.error(ident, "ident does not belong to an input/query param")
    }
  def getQuery(ident: Ident)(using ParseContext): ParseResult[VariableReference.QueryLike] =
    find(ident) match {
      case Some(queryRef: VariableReference.QueryLike) => ParseResult.Success(queryRef)
      case Some(_: VariableReference.InputLike)        => ParseResult.error(ident, "ident belongs to an input param, but a query param is expected")
      case None                                        => ParseResult.error(ident, "ident does not belong to an input/query param")
    }

  def getRootQueryRef(errorPos: Tree)(using ParseContext): ParseResult[VariableReference.QueryLike] =
    map.valuesIterator.collect { case ref: VariableReference.QueryLike if ref.isRoot => ref }.toList match
      case ref :: Nil => ParseResult.Success(ref)
      case Nil        => ParseResult.error(errorPos, "no root param found?")
      case _          => ParseResult.error(errorPos, "many root params found?")

}
private object RefMap {
  val empty: RefMap = new RefMap(Map.empty)
}
