package oxygen.sql.generic

import oxygen.quoted.*

private[generic] final class RefMap private (map: Map[Symbol, QueryReference]) {
  def get(sym: Symbol): Option[QueryReference] = map.get(sym)
  def add(elems: (Symbol, QueryReference)*): RefMap = new RefMap(map ++ elems.toMap)
  def addList(elems: List[(Symbol, QueryReference)]): RefMap = new RefMap(map ++ elems.toMap)
}
private object RefMap {
  val empty: RefMap = new RefMap(Map.empty)
}
