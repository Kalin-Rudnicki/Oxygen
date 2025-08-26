package oxygen.example.ui.page.index

import org.scalajs.dom.CanvasRenderingContext2D
import oxygen.example.ui.page.other.OtherPage
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.{*, given}
import oxygen.ui.web.defaults.PageErrorsBottomCorner
import zio.*

object IndexPage extends RoutablePage[Any] {

  final case class Params(
      initialUrl: List[String],
      count: Option[Int],
  )

  override lazy val paramCodec: PageCodec[Params] =
    (PageCodec.path.rest.string / PageCodec.query.plain.optional[Int]("count")).transform(Params.apply, p => (p.initialUrl, p.count))

  final case class State(
      pagePath: List[String],
      count: Int,
      postLoadHit: Boolean,
  )

  override def paramsFromState(state: State): Params =
    Params(state.pagePath, state.count.some)

  private val messages: Chunk[PageMessage] =
    for {
      t <- Chunk.from(PageMessage.Type.values.toSeq)
      s <- Chunk(
        s"$t",
        s"$t : lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
        s"$t : lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
      )
    } yield PageMessage.make(t, s)

  override def initialLoad(params: Params): ZIO[Scope, UIError, State] =
    for {
      _ <- ZIO.logWarning("Sleeping on Index page load")
      _ <- Clock.sleep(3.seconds)
      // _ <- ZIO.fail(UIError.ExternalError("oopsie"))
    } yield State(params.initialUrl, params.count.getOrElse(0), false)

  override def postLoad(state: State, rh: RaiseHandler.Stateful[Nothing, State]): ZIO[Scope, UIError, Unit] = {
    Clock.sleep(2.seconds) *>
      PageMessages.PageLocal.updateCurrentPage(_ :++ messages) *>
      rh.updateState(_.copy(postLoadHit = true)) *>
      Clock.sleep(3.seconds) *>
      rh.updateState(state => state.copy(count = state.count + 10))
  }

  override def title(state: State): String = "Index"

  override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
    div(
      PageErrorsBottomCorner.lifted,
      h1("Index"),
      verticalSpacing,
      Read(),
      verticalSpacing,
      Write().convertAction { inc => Raise.updateState[Int](_ + inc.i) }.zoomOut[State](_.count),
      verticalSpacing,
      GoToOther(),
      verticalSpacing,
      canvas { (ctx: CanvasRenderingContext2D) =>
        ctx.fillStyle = "black"
        ctx.fillRect(0, 0, 500, 500)
        ctx.fillStyle = "red"
        ctx.fillRect(state.count, state.count, 75, 75)
      }(
        // width := "100%",
        // height := "100%",
        htmlWidth := 500,
        htmlHeight := 500,
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val verticalSpacing: Widget.Const =
    div(height := "10px")

  private val horizontalSpacing: Widget.Const =
    div(display.inlineBlock, width := "10px")

  private object Read extends Component.WithoutProps.Stateful[Any, Nothing, State] {

    override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
      div(
        p(s"page-path: ${state.pagePath.mkString("/", "/", "")}"),
        p(s"count: ${state.count}"),
        p(s"post-load-hit: ${state.postLoadHit}"),
      )

  }

  private object Write extends Component.WithoutProps.Stateless[Any, Inc] {

    override protected val component: Widget.Stateless[Any, Inc] =
      div(
        MyButton(-5),
        horizontalSpacing,
        MyButton(-1),
        horizontalSpacing,
        MyButton(1),
        horizontalSpacing,
        MyButton(5),
      )

  }

  private object GoToOther extends Component.WithoutProps.Stateful[Any, Nothing, State] {

    override protected def component(state: State): Widget.Stateful[Any, Nothing, State] =
      button(
        "Go to other page",
        onClick := OtherPage.navigate.push(OtherPage.Params(state.count)),
      )

  }

  final case class Inc(i: Int)

  private object MyButton extends Component.WithProps.Stateless[Any, Inc] {

    override type Props = Int

    override protected def component(props: Int): Widget.Stateless[Any, Inc] =
      button(
        if (props < 0) props.toString
        else s"+$props",
        onClick.action(Inc(props)),
      )

  }

}
