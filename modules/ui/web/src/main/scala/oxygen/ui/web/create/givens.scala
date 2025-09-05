package oxygen.ui.web.create

import scala.reflect.TypeTest

given nothingTypeTest: [A] => TypeTest[A | Nothing, Nothing] = _ => None
