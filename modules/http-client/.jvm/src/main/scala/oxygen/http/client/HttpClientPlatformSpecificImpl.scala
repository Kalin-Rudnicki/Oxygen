package oxygen.http.client

trait HttpClientPlatformSpecificImpl { self: HttpClientPlatformSpecific =>

  override def defaultClient(target: ConnectionTarget): HttpClient =
    JvmHttpClient(target)

}
