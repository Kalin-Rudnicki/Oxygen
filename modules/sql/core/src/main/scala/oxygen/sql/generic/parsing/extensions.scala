package oxygen.sql.generic.parsing

import oxygen.quoted.*

extension (self: Term)
  private[generic] def simplify: Term = self.underlying match
    case Typed(term, _) => term.simplify
    case _              => self
