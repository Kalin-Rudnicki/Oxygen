package oxygen.slyce.core.node

import oxygen.slyce.core.*

// FIX-PRE-MERGE (KR) : this is definitely going to require some adjustments...
//                    : with a case class its quite easy to add an annotation to the class to say `@ignoreBetween[XYZ]`, but not sure how to do this with lists...
//                    : in a list there seems to be 4 parts: [ A ] [ A _ , __ A ] [ A _ , __ A _ , __ A ]
//                    : here, there is `A`, `_`, `__`, and `,`.
//                    : if you only care about `A`, then you could have a sort of `MyList[A, (Option['_'], ',', Option['__'])]`,
//                    : but what if you care about the `,`

sealed trait ListNode[+A, +B] extends Node

final case class EmptyListNode() extends ListNode[Nothing, Nothing]

final case class NonEmptyListNode[+A, +B](value: A, tail: NonEmptyListNode.TailNode[A, B]) extends ListNode[A, B]
object NonEmptyListNode {

  sealed trait TailNode[+A, +B] extends Node

}
