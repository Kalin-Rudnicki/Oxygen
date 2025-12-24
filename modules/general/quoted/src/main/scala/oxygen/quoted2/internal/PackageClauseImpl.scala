package oxygen.quoted2.internal

import oxygen.quoted2.*
import scala.quoted.*

private[quoted2] final class PackageClauseImpl(using val quotes: Quotes)(val unwrap: quotes.reflect.PackageClause) extends PackageClause {

  // FIX-PRE-MERGE (KR) :

}
private[quoted2] object PackageClauseImpl {

  final class Companion(using val quotes: Quotes) extends PackageClause.Companion {

    // FIX-PRE-MERGE (KR) :

  }

}
