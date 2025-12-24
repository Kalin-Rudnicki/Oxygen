package oxygen.quoted2

import scala.quoted.*

trait Tree private[quoted2] () {

  // FIX-PRE-MERGE (KR) :

}
object Tree {

  def wrap(using quotes: Quotes)(tree: quotes.reflect.Tree): Tree = ???

  // FIX-PRE-MERGE (KR) :
  
  trait Companion {

    // FIX-PRE-MERGE (KR) :  
    
  }

}
