package oxygen.slyce.core

sealed trait Element {

  // this prevents something extending Token and Node
  val elementType: String

  val elementName: String

}

trait Token extends Element {

  override final val elementType: String = "Token"

  // FIX-PRE-MERGE (KR) :

}

trait Node extends Element {

  override final val elementType: String = "Node"

  // FIX-PRE-MERGE (KR) :

}
