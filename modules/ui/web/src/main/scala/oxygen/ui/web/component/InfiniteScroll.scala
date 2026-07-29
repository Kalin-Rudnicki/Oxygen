package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Intersect

/**
  * W6-T03: infinite / load-more list helper (not virtualization).
  *
  * Pure [[State]] drives Idle → Loading → (append | End | Failed).
  * [[footer]] keeps a stable `#oxy-infinite-scroll-sentinel` in the DOM for
  * IntersectionObserver (id never unmounts across phase changes).
  * Wire [[Intersect.observeId]] for auto load-more; Load more / Retry remain for a11y.
  *
  * TODO (KR): very much WIP / API feels wonky — revisit after core cleanup.
  *
  * {{{
  * case class S(feed: InfiniteScroll.State[Row] = InfiniteScroll.State.empty)
  * // …
  * InfiniteScroll.footer[Row].zoomOut[S](_.feed)
  * // on load: beginLoad → fetch → append(batch, hasMore)
  * }}}
  */
object InfiniteScroll {

  enum Phase {
    case Idle
    case Loading
    case End
    case Failed(message: String)
  }

  final case class State[A](
      items: Vector[A],
      phase: Phase,
      /** 0-based next page index for page-style loaders. */
      nextPage: Int,
  ) {
    def canLoadMore: Boolean = phase == Phase.Idle
    def isLoading: Boolean = phase == Phase.Loading
    def isEnd: Boolean = phase == Phase.End
    def isFailed: Boolean = phase match {
      case Phase.Failed(_) => true
      case _               => false
    }
    def failureMessage: Option[String] = phase match {
      case Phase.Failed(m) => Some(m)
      case _               => None
    }

    /** Transition Idle → Loading; no-op if not idle. */
    def beginLoad: State[A] =
      if canLoadMore then copy(phase = Phase.Loading) else this

    /** Append a page; set End when `hasMore` is false. */
    def append(more: Seq[A], hasMore: Boolean): State[A] =
      copy(
        items = items ++ more.toVector,
        phase = if hasMore then Phase.Idle else Phase.End,
        nextPage = nextPage + 1,
      )

    def fail(message: String): State[A] =
      copy(phase = Phase.Failed(message))

    /** Failed → Idle so the user / sentinel can retry. */
    def retry: State[A] =
      phase match {
        case Phase.Failed(_) => copy(phase = Phase.Idle)
        case _               => this
      }

    def markEnd: State[A] = copy(phase = Phase.End)
  }
  object State {
    def empty[A]: State[A] = State(Vector.empty, Phase.Idle, 0)
    def of[A](items: Seq[A], hasMore: Boolean = true): State[A] =
      State(items.toVector, if hasMore then Phase.Idle else Phase.End, 1)
  }

  /**
    * Sentinel footer for `WidgetState[State[A]]`.
    * Stable `id` = [[Intersect.infiniteScrollSentinelId]] so observers survive re-renders.
    */
  def footer[A]: WidgetS[State[A]] =
    Widget.state[State[A]].fix { st =>
      val s = st.renderTimeValue
      div(
        id := Intersect.infiniteScrollSentinelId,
        padding := S.spacing._4,
        textAlign.center,
        minHeight := 48.px,
        s.phase match {
          case Phase.Loading =>
            div(
              display.inlineFlex,
              alignItems.center,
              justifyContent.center,
              gap := S.spacing._2,
              color := S.color.fg.moderate,
              Spinner.small,
              span("Loading more…"),
            )
          case Phase.End =>
            span(color := S.color.fg.subtle, s"End of list · ${s.items.size} items")
          case Phase.Failed(msg) =>
            div(
              display.flex,
              flexDirection.column,
              alignItems.center,
              gap := S.spacing._2,
              span(color := S.color.status.negative.standard, msg),
              Button("Retry")
                .small
                .subtle
                .content(onClick := st.update(_.retry.beginLoad)),
            )
          case Phase.Idle =>
            Button("Load more")
              .small
              .subtle
              .content(onClick := st.update(_.beginLoad))
        },
      )
    }

}
