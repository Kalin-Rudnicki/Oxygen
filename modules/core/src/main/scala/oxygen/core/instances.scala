package oxygen.core

import scala.annotation.tailrec

object instances {

  given listOrd: [A: Ordering as ord] => Ordering[List[A]] =
    new Ordering[List[A]] {
      @tailrec
      override def compare(x: List[A], y: List[A]): Int =
        (x, y) match {
          case (xHead :: xTail, yHead :: yTail) =>
            ord.compare(xHead, yHead) match {
              case 0   => compare(xTail, yTail)
              case res => res
            }
          case (_ :: _, Nil) => 1
          case (Nil, _ :: _) => -1
          case (Nil, Nil)    => 0
        }
    }

}
