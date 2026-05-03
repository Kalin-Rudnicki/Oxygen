package oxygen.example.ui.electron

import oxygen.example.ui.electron.page as P
import oxygen.ui.web.*
import oxygen.ui.web.create.*
import oxygen.ui.web.defaults.*
import scala.collection.immutable.ArraySeq
import scala.scalajs.js.annotation.JSExportTopLevel
import zio.*

@JSExportTopLevel("renderer")
object Renderer extends PageApp[Renderer.Env] {

  type Env =
    Any

  // override val logLevel: LogLevel = LogLevel.Debug

  /*
  override val jobs: Seq[GlobalJob[Env]] =
    Seq(
      GlobalJob.simplePoll("say-hi", 15.seconds)(5.seconds) {
        PageMessages.schedule(PageMessage.info("Hi there!"), 2.seconds)
      },
    )
   */

  override val styleSheets: ArraySeq[StyleSheet] = ArraySeq(
    normalizeCssReset,
    OxygenStyleVars.toCSS(OxygenStyleVarDefaults.CZR),
    InlinePseudoClassStyles.compiled,
    OxygenStyleSheet.compiled,
  )

  override val pages: ArraySeq[RoutablePage[Env]] = ArraySeq(
    P.IndexPage,
    P.OtherPage,
    StylesPage,
    ComponentsPage,
  )

  override def layer: TaskLayer[Env] =
    ZLayer.empty

}
