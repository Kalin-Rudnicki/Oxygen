package oxygen.ui.web

import oxygen.ui.web.create.Widget
import zio.*

/**
  * State-tied page/region lock (W4). Nested acquire counts; Scope release unlocks.
  */
final case class LockState(
    pageCount: Int,
    regions: Map[String, Int],
) {
  def pageLocked: Boolean = pageCount > 0
  def regionLocked(id: String): Boolean = pageLocked || regions.getOrElse(id, 0) > 0
  def anyLocked: Boolean = pageLocked || regions.values.exists(_ > 0)

  /** Whether controls should disable for the given optional region id (None = page). */
  def shouldDisable(region: Option[String]): Boolean =
    region match
      case None     => pageLocked
      case Some(id) => regionLocked(id)
}
object LockState {
  val empty: LockState = LockState(0, Map.empty)
}

object PageLock extends PageLocalState[LockState]("PageLock")(LockState.empty) {

  def isPageLocked: UIO[Boolean] =
    get.map(_.pageLocked)

  def isRegionLocked(id: String): UIO[Boolean] =
    get.map(_.regionLocked(id))

  /** Acquire page lock; released when the surrounding Scope closes. */
  def acquirePage: URIO[Scope, Unit] =
    for {
      _ <- update(s => s.copy(pageCount = s.pageCount + 1))
      _ <- ZIO.addFinalizer(update(s => s.copy(pageCount = (s.pageCount - 1).max(0))))
    } yield ()

  /** Acquire named region lock; released when Scope closes. */
  def acquireRegion(id: String): URIO[Scope, Unit] =
    for {
      _ <- update { s =>
        val n = s.regions.getOrElse(id, 0) + 1
        s.copy(regions = s.regions.updated(id, n))
      }
      _ <- ZIO.addFinalizer {
        update { s =>
          val n = s.regions.getOrElse(id, 0) - 1
          if n <= 0 then s.copy(regions = s.regions - id)
          else s.copy(regions = s.regions.updated(id, n))
        }
      }
    } yield ()

  /**
    * Run `effect` under a page lock. Scope is contained — callers need not wrap in `ZIO.scoped`.
    */
  def withPageLock[R, E, A](effect: ZIO[R & Scope, E, A]): ZIO[R, E, A] =
    ZIO.scoped[R] {
      acquirePage *> effect
    }

  /**
    * Run `effect` under a region lock. Scope is contained — callers need not wrap in `ZIO.scoped`.
    */
  def withRegionLock[R, E, A](id: String)(effect: ZIO[R & Scope, E, A]): ZIO[R, E, A] =
    ZIO.scoped[R] {
      acquireRegion(id) *> effect
    }

  // TODO (KR) : this also requires some looking at...

  /**
    * W4-T04: rebuild `f` when lock changes. Inner widget is detached from lock state
    * so forms/pages keep their own state types.
    */
  def bind[Env, Action, StateGet, StateSet <: StateGet](
      f: LockState => Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    detach { lockWs => f(lockWs.renderTimeValue) }

  def bindPage[Env, Action, StateGet, StateSet <: StateGet](
      f: Boolean => Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    bind(s => f(s.pageLocked))

  def bindRegion[Env, Action, StateGet, StateSet <: StateGet](
      id: String,
  )(
      f: Boolean => Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    bind(s => f(s.regionLocked(id)))

}
