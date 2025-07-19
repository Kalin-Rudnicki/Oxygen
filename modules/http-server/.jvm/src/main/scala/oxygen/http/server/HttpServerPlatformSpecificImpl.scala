package oxygen.http.server

trait HttpServerPlatformSpecificImpl { self: HttpServerPlatformSpecific =>

  override def defaultServer: HttpServer = JvmHttpServer()

}
