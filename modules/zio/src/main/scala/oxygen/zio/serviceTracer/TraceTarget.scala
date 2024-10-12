package oxygen.zio.serviceTracer

import zio.UIO

trait TraceTarget {

  val name: String
  val handle: TraceElem => UIO[Unit]

  // TODO (KR) : toString

}
