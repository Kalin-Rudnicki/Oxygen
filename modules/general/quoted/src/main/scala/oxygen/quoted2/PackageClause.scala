package oxygen.quoted2

import scala.quoted.*

trait PackageClause private[quoted2] () extends Tree {

  // FIX-PRE-MERGE (KR) :

}
object PackageClause {

  def wrap(using quotes: Quotes)(packageClause: quotes.reflect.PackageClause): PackageClause = ???

  // FIX-PRE-MERGE (KR) :

  trait Companion {

    // FIX-PRE-MERGE (KR) :

  }

}
