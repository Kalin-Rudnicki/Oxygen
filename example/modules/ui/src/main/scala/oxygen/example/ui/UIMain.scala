package oxygen.example.ui

import oxygen.example.ui.page as P
import oxygen.ui.web.*
import scala.collection.immutable.ArraySeq
import zio.*

object UIMain extends PageApp[Any] {

  // override val logLevel: LogLevel = LogLevel.Trace

  override val pages: ArraySeq[RoutablePage[Any]] = ArraySeq(
    P.other.OtherPage,
    P.index.IndexPage,
  )

  override def layer: TaskLayer[Any] =
    ZLayer.empty

}
