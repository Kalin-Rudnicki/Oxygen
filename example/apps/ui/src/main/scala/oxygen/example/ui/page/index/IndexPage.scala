package oxygen.example.ui.page.index

import java.time.Instant
import java.util.UUID
import oxygen.example.api.StreamApi
import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*
import zio.stream.*

object IndexPage extends RoutablePage.NoParams[LocalService & StreamApi] {

  final case class PageState(
      userToken: Option[UserToken],
      uuid: Option[ReceivedEvent],
  ) {

    def user: Option[User] = userToken.map(_.user)

  }

  final case class ReceivedEvent(
      uuid: UUID,
      generatedAt: Instant,
      receivedAt: Instant,
  ) {

    override def toString: String =
      s"$uuid (delay=${Duration.fromInterval(generatedAt, receivedAt).render})"

  }

  override def initialLoad(params: PageParams): ZIO[LocalService & Scope, UIError, PageState] =
    for {
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.getOption)
    } yield PageState(userToken, None)

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope & StreamApi, UIError, Unit] =
    for {
      _ <- Clock.sleep(5.seconds)
      _ <-
        (ZStream.scoped { ZIO.addFinalizer(ZIO.logWarning("I have been killed!")) } *> StreamApi.randomUUIDs).foreach { id =>
          Clock.instant.flatMap { now =>
            state.update(_.copy(uuid = ReceivedEvent(id.id, id.timestamp, now).some))
          }
        }.forkScoped
    } yield ()

  override def title(state: PageState): String = "Index"

  override val path: Seq[String] = Seq()

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService, PageState] =
    PageLayout.layout(optionalSignedInNavBar(renderState.user))(
      PageMessagesBottomCorner.default,
      h1("Oxygen Example"),
      Section.section1()(
        s"Id: ${renderState.uuid.fold("N/A")(_.toString)}",
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
