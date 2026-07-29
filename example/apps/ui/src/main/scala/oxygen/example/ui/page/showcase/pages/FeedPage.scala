package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Intersect
import zio.*

object FeedPage extends RoutablePage.NoParams[Any] {
  final case class FeedItem(id: Int, title: String, detail: String)

  /** Tall-enough first page so the document scrolls past the viewport. */
  private val initialCount: Int = 40
  private val pageSize: Int = 20
  private val maxItems: Int = 120

  private def item(i: Int): FeedItem =
    FeedItem(i, s"Activity #$i", s"Mock event · user-${i % 7} · ${if i % 3 == 0 then "deploy" else if i % 3 == 1 then "login" else "invoice"}")

  private def batch(from: Int, count: Int): Vector[FeedItem] =
    (from until from + count).map(item).toVector

  final case class PageState(
      feed: InfiniteScroll.State[FeedItem] = InfiniteScroll.State.of(batch(0, initialCount), hasMore = true),
  )

  override val path: Seq[String] = Seq("showcase", "data", "feed")
  override def title(s: PageState): String = "Infinite feed"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())

  /**
    * Auto load-more when the footer sentinel enters (or nears) the viewport.
    * Completes Loading with a mock delay; toast explains when the trigger fired.
    */
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    for {
      rt <- ZIO.runtime[Any]
      _ <- Intersect.observeId(Intersect.infiniteScrollSentinelId, once = false, rootMargin = "240px") { () =>
        // fire-and-forget from the IO callback into the page runtime
        given Unsafe = Unsafe.unsafe(identity)
        rt.unsafe.fork {
          completeLoadMore(state).catchAllCause(c => ZIO.logWarningCause("feed load-more", c))
        }
        ()
      }
    } yield ()

  /** Shared path for button click + intersection: toast → Loading → mock fetch → append. */
  private def completeLoadMore(state: WidgetState[PageState]): UIO[Unit] =
    state.currentValue.flatMap { s =>
      if !s.feed.canLoadMore then ZIO.unit
      else {
        val from = s.feed.items.size
        val page = s.feed.nextPage
        val remaining = maxItems - from
        for {
          _ <- state.update(ps => ps.copy(feed = ps.feed.beginLoad))
          _ <- PageMessages.schedule(
            PageMessage.info(s"Load more fired · page $page · fetching from #$from"),
            2.seconds,
          )
          _ <- ZIO.sleep(450.millis) // mock network
          _ <- state.update { ps =>
            if !ps.feed.isLoading then ps
            else if remaining <= 0 then ps.copy(feed = ps.feed.markEnd)
            else {
              val n = math.min(pageSize, remaining)
              ps.copy(feed = ps.feed.append(batch(from, n), hasMore = from + n < maxItems))
            }
          }
        } yield ()
      }
    }

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(FeedPage, "Infinite feed")(
        ShowcaseLayout.todoBackend("Paged activity API"),
        ShowcaseLayout.note(
          s"Starts with $initialCount rows (scroll the page). Near the bottom, IntersectionObserver " +
            s"fires load-more (+ toast). Manual “Load more” works too. Caps at $maxItems items.",
        ),
        div(
          fontSize := S.fontSize._2,
          color := S.color.fg.moderate,
          marginBottom := S.spacing._3,
          s"${renderState.feed.items.size} items loaded · phase ${renderState.feed.phase}",
        ),
        // list
        div(
          border(1.px, "solid", S.color.bg.layerThree),
          borderRadius := S.borderRadius._3,
          overflow.hidden,
          Widget.foreach(renderState.feed.items.toList) { row =>
            div(
              display.flex,
              alignItems.center,
              justifyContent.spaceBetween,
              gap := S.spacing._3,
              padding := css(S.spacing._3, S.spacing._4),
              borderBottom(1.px, "solid", S.color.bg.layerThree),
              // roomy rows so 40 items >> one viewport
              minHeight := 56.px,
              div(
                div(fontWeight := "600", color := S.color.fg.default, row.title),
                div(fontSize := S.fontSize._2, color := S.color.fg.subtle, row.detail),
              ),
              span(fontSize := S.fontSize._1, color := S.color.fg.minimal, f"#${row.id}%03d"),
            )
          },
        ),
        // footer: beginLoad on click; postLoad observer + this handler complete the fetch
        Widget.state[PageState].fix { st =>
          val feed = st.renderTimeValue.feed
          div(
            id := Intersect.infiniteScrollSentinelId,
            padding := S.spacing._6,
            textAlign.center,
            minHeight := 64.px,
            feed.phase match {
              case InfiniteScroll.Phase.Loading =>
                div(
                  display.inlineFlex,
                  alignItems.center,
                  gap := S.spacing._2,
                  Spinner.small,
                  span(color := S.color.fg.moderate, "Loading more…"),
                )
              case InfiniteScroll.Phase.End =>
                span(color := S.color.fg.subtle, s"End of list · ${feed.items.size} items")
              case InfiniteScroll.Phase.Failed(msg) =>
                div(
                  display.flex,
                  flexDirection.column,
                  alignItems.center,
                  gap := S.spacing._2,
                  span(color := S.color.status.negative.standard, msg),
                  Button("Retry").small.subtle.content(onClick := completeLoadMore(st)),
                )
              case InfiniteScroll.Phase.Idle =>
                Button("Load more")
                  .small
                  .subtle
                  .content(onClick := completeLoadMore(st))
            },
          )
        },
      )

}
