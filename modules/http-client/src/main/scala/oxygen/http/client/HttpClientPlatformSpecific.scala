package oxygen.http.client

trait HttpClientPlatformSpecific {

  def defaultClient(target: ConnectionTarget): HttpClient

}
