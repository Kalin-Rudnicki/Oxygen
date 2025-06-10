package oxygen.core.generic

import oxygen.core.typeclass.*

extension (self: Show.type)
  inline def derived[A]: Show[A] =
    ShowGeneric.derived[A]
