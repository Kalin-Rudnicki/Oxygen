package oxygen.http.core

import oxygen.http.model.*
import oxygen.meta.*

given ToExprT[HttpCode] = ToExprT.derived
given ToExprT[HttpMethod.Standard] = ToExprT.derived
given ToExprT[DecodingFailure.Source] = ToExprT.derived
