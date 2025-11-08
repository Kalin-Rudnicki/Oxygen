package oxygen.example.ui.page

import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Window
import zio.*

// FIX-PRE-MERGE (KR) : remove
object RingPage extends RoutablePage.NoParams[LocalService] {

  final case class PageState(
      userToken: Option[UserToken],
      rings: Seq[Ring],
      selected: Option[Ring],
      inputForm: Ring,
  ) {

    def user: Option[User] = userToken.map(_.user)

  }

  final case class Ring(
      cut: String,
      carat: String,
      length: String,
      width: String,
      height: String,
  ) {

    def urlFormat: String =
      s"${carat}ct-$cut-${length}x${width}x$height"

  }
  object Ring {

    val empty: Ring = Ring("", "", "", "", "")

  }

  override def initialLoad(params: PageParams): ZIO[LocalService & Scope, UIError, PageState] =
    for {
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.getOption)
    } yield PageState(userToken, Nil, None, Ring.empty)

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Rings"

  override val path: Seq[String] = Seq("rings")

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService, PageState] =
    PageLayout.layout(optionalSignedInNavBar(renderState.user))(
      PageMessagesBottomCorner.attached,
      h1("Rings"),
      ringTable2,
      fullForm,
    )

  private lazy val ringTable2: WidgetS[PageState] =
    Widget.state[PageState].get { state =>
      ringTable(state.selected).zoomOut[PageState](_.rings).handleActionStateful.s { (state, ring) =>
        state.currentValue.flatMap {
          _.selected match {
            case Some(selected) =>
              Window.newTab(s"https://www.diamdb.com/compare/${selected.urlFormat}-vs-${ring.urlFormat}") *>
                state.update(_.copy(selected = None))
            case None =>
              state.update(_.copy(selected = ring.some))
          }
        }
      }
    }

  private def ringTable(selected: Option[Ring]): WidgetAS[Ring, Seq[Ring]] =
    Section.section1("Rings")(
      Table.basic(_.centerAlignCells)(
        tr(
          th("*"),
          th("Cut"),
          th("Carat"),
          th("Dimensions"),
          th("Actions"),
        ),
        Widget.seq[Seq](ringRow(selected)),
      ),
    )

  private def ringRow(selected: Option[Ring]): WidgetAS[Ring, Ring] =
    Widget.state[Ring].get { ring =>
      tr(
        td(
          color.red,
          Widget.when(selected.contains(ring))("*"),
        ),
        td(ring.cut),
        td(ring.carat),
        td(s"${ring.length} x ${ring.width} x ${ring.height}"),
        td(
          Button("Select", _.extraSmall)(
            onClick.a[Ring].action(ring),
          ),
        ),
      )
    }

  private lazy val rawForm: SubmitFormS[Ring, Ring] =
    (
      Form.textField[String]("cut").required.zoomOut[Ring](_.cut) <*>
        Form.textField[String]("carat").required.zoomOut[Ring](_.carat) <*>
        Form.textField[String]("length").required.zoomOut[Ring](_.length) <*>
        Form.textField[String]("width").required.zoomOut[Ring](_.width) <*>
        Form.textField[String]("height").required.zoomOut[Ring](_.height) <*>
        Form.submitButton("Add Ring")
    ).mapValue(Ring.apply)

  private lazy val fullForm: WidgetS[PageState] =
    Section.section1("Add Ring")(
      rawForm.zoomOut[PageState](_.inputForm).onSubmit.s { (state, _, ring) =>
        state.update(_.copy(rings = state.get.rings :+ ring, inputForm = Ring.empty))
      },
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
