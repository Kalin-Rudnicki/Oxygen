package oxygen.http.core

import oxygen.http.model.*
import zio.*

trait ResponseMiddleware {
  def map(response: HttpResponse): URIO[Scope, HttpResponse]
}
