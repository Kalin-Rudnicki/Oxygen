package oxygen.ui.web

/**
  * W6-T01: explicit memo for pure rebuilds (above Renderer; not a second VDOM).
  *
  * Prefer this when a subtree is expensive and depends on a small key derived from state.
  * Invalidates when `key` changes; concurrent reads are single-threaded on the UI fiber.
  *
  * TODO (KR): not verified or used in production paths yet — treat as experimental.
  *
  * {{{
  * private val bodyMemo = Memo[Int, Widget]()
  * // in build:
  * bodyMemo(pageState.counter) { c => expensiveWidget(c) }
  * }}}
  */
final class Memo[K, V] {
  private var lastKey: Option[K] = None
  private var lastValue: Option[V] = None
  private var hits: Long = 0
  private var misses: Long = 0

  def apply(key: K)(build: K => V): V =
    lastKey match {
      case Some(k) if k == key =>
        hits += 1
        lastValue.get
      case _ =>
        misses += 1
        val v = build(key)
        lastKey = Some(key)
        lastValue = Some(v)
        v
    }

  def clear(): Unit = {
    lastKey = None
    lastValue = None
  }

  def stats: Memo.Stats = Memo.Stats(hits, misses)

  def hitCount: Long = hits
  def missCount: Long = misses
}
object Memo {
  final case class Stats(hits: Long, misses: Long) {
    def total: Long = hits + misses
  }

  def apply[K, V](): Memo[K, V] = new Memo[K, V]
}
