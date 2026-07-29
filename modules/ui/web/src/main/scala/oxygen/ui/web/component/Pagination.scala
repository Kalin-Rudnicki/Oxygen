package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W6-T02: page / pageSize / total pagination state + chrome.
  */
object Pagination {

  final case class State(
      page: Int, // 0-based
      pageSize: Int,
      total: Int,
  ) {
    def pageCount: Int =
      if pageSize <= 0 then 0
      else if total <= 0 then 0
      else (total + pageSize - 1) / pageSize

    def pageClamped: Int = {
      val max = (pageCount - 1).max(0)
      page.max(0).min(max)
    }

    def offset: Int = pageClamped * pageSize

    def slice[A](items: Seq[A]): Seq[A] =
      items.slice(offset, offset + pageSize)

    def canPrev: Boolean = pageClamped > 0
    def canNext: Boolean = pageClamped < pageCount - 1

    def prev: State = copy(page = (pageClamped - 1).max(0))
    def next: State = copy(page = (pageClamped + 1).min((pageCount - 1).max(0)))
    def withPage(p: Int): State = copy(page = p)
    def withPageSize(n: Int): State = copy(pageSize = n.max(1), page = 0)
    def withTotal(t: Int): State = copy(total = t.max(0))
  }
  object State {
    def initial(pageSize: Int = 10, total: Int = 0): State =
      State(0, pageSize.max(1), total.max(0))
  }

  /** Prev / page indicator / Next controls bound to [[State]]. */
  def controls: WidgetS[State] =
    Widget.state[State].fix { st =>
      val s = st.renderTimeValue
      val pageLabel = s"${s.pageClamped + 1} / ${s.pageCount.max(1)}"
      div(
        display.flex,
        alignItems.center,
        gap := S.spacing._3,
        Button("Prev")
          .small
          .subtle
          .disabled(!s.canPrev)
          .content(onClick := st.update(_.prev)),
        span(pageLabel, color := S.color.fg.moderate),
        Button("Next")
          .small
          .subtle
          .disabled(!s.canNext)
          .content(onClick := st.update(_.next)),
        span(s"${s.total} items", color := S.color.fg.subtle, fontSize := S.fontSize._2),
      )
    }

}
