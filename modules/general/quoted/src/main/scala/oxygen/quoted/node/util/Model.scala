package oxygen.quoted.node.util

import scala.quoted.*

trait Model private[node] () {

  private[node] val quotes: Quotes
  private[node] val unwrap: Any
  
}
