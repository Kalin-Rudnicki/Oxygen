package oxygen.http.core.generic

import oxygen.meta.*
import zio.http.{Method, Status}

given ToExprT[Status] = ToExprT.derived
given ToExprT[Method] = ToExprT.derived
