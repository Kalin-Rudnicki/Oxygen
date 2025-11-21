package oxygen.ui.web

import monocle.Lens
import oxygen.predef.core.*
import oxygen.ui.web.internal.*
import zio.*

sealed trait PWidgetState[+StateGet, -StateSet <: StateGet] {

  val renderTimeValue: StateGet
  def unsafeCurrentValue: StateGet

  final lazy val get: StateGet = renderTimeValue
  final def read: UIO[StateGet] = currentValue
  final def currentValue: UIO[StateGet] = ZIO.succeed { unsafeCurrentValue }

  private[web] def setInternal(value: StateSet): UIO[Unit]
  private[web] def updateInternal(f: StateGet => StateSet): UIO[Unit]
  private[web] def reRenderInternal: UIO[Unit]

  final def set(value: StateSet): UIO[Unit] = setInternal(value) *> reRenderInternal
  final def update(f: StateGet => StateSet): UIO[Unit] = updateInternal(f) *> reRenderInternal

  final def setNoReRender(value: StateSet): UIO[Unit] = setInternal(value)
  final def updateNoReRender(f: StateGet => StateSet): UIO[Unit] = updateInternal(f)

}
object PWidgetState {

  type Anything = PWidgetState[Any, Nothing]
  type Fixed[S] = PWidgetState[S, S]

  extension [S](self: PWidgetState[S, S])
    private[web] def fix: WidgetState[S] =
      self match
        case self: WidgetState[S] => self

  case object Stateless extends PWidgetState[Any, Nothing] {
    override val renderTimeValue: Any = null
    override def unsafeCurrentValue: Any = throw new RuntimeException("stateless widget")
    override private[web] def setInternal(value: Nothing): UIO[Unit] = ZIO.dieMessage("stateless widget")
    override private[web] def updateInternal(f: Any => Nothing): UIO[Unit] = ZIO.dieMessage("stateless widget")
    override private[web] def reRenderInternal: UIO[Unit] = ZIO.dieMessage("stateless widget")
  }

}

trait WidgetState[S] extends PWidgetState[S, S] {

  inline final def zoomIn[S2](inline f: S => S2): WidgetState[S2] =
    WidgetState.ZoomIn(this, LensUtil.genLens(f))

  final def zoomInLens[S2](lens: Lens[S, S2]): WidgetState[S2] =
    WidgetState.ZoomIn(this, lens)

}
object WidgetState {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class GlobalValue[State](
      state: GlobalStateManager.Value[State],
      pageInstance: PageInstance.Untyped,
  ) extends WidgetState[State] {

    override val renderTimeValue: State = state.get()
    override def unsafeCurrentValue: State = state.get()

    override private[web] def setInternal(value: State): UIO[Unit] = ZIO.succeed { state.set(value) }
    override private[web] def updateInternal(f: State => State): UIO[Unit] = ZIO.succeed { state.modify(f) }
    override private[web] def reRenderInternal: UIO[Unit] = pageInstance.render(NavigationEvent.NavType.Replace)

  }
  object GlobalValue {
    def fromGlobalState[State](state: GlobalState[State], pageInstance: PageInstance.Untyped): WidgetState[State] = GlobalValue(state.getValue(), pageInstance)
    def fromPageLocalState[State](state: PageLocalState[State], pageInstance: PageInstance.Untyped): WidgetState[State] = GlobalValue(state.getValue(pageInstance.pageReference), pageInstance)
  }

  final case class ZoomIn[OuterState, InnerState](
      outer: WidgetState[OuterState],
      lens: Lens[OuterState, InnerState],
  ) extends WidgetState[InnerState] {

    override val renderTimeValue: InnerState = lens.get(outer.renderTimeValue)
    override def unsafeCurrentValue: InnerState = lens.get(outer.unsafeCurrentValue)

    override private[web] def setInternal(value: InnerState): UIO[Unit] = outer.updateInternal(lens.replace(value))
    override private[web] def updateInternal(f: InnerState => InnerState): UIO[Unit] = outer.updateInternal(lens.modify(f))
    override private[web] def reRenderInternal: UIO[Unit] = outer.reRenderInternal

  }

  final case class SumCase[OuterState, InnerState <: OuterState](
      outer: WidgetState[OuterState],
      pf: PartialFunction[OuterState, InnerState],
  ) extends WidgetState[InnerState] {

    override val renderTimeValue: InnerState = pf.applyOrElse(outer.renderTimeValue, _ => throw new RuntimeException("Attempted to create SumCase with invalid OuterState"))
    override def unsafeCurrentValue: InnerState = pf.applyOrElse(outer.unsafeCurrentValue, _ => throw new RuntimeException("Attempted to create SumCase with invalid OuterState"))

    override private[web] def setInternal(value: InnerState): UIO[Unit] = outer.setInternal(value)
    override private[web] def updateInternal(f: InnerState => InnerState): UIO[Unit] =
      outer.updateInternal {
        case pf(inner) => f(inner)
        case _         => throw new RuntimeException("Attempted to update SumCase with invalid OuterState")
      }
    override private[web] def reRenderInternal: UIO[Unit] = outer.reRenderInternal

  }

  final case class Zipped[A, B, C](a: WidgetState[A], b: WidgetState[B], zip: Zip.Out[A, B, C]) extends WidgetState[C] {

    override val renderTimeValue: C = zip.zip(a.renderTimeValue, b.renderTimeValue)
    override def unsafeCurrentValue: C = zip.zip(a.unsafeCurrentValue, b.unsafeCurrentValue)

    override private[web] def setInternal(value: C): UIO[Unit] = {
      val (aValue, bValue) = zip.unzip(value)
      a.setInternal(aValue) *> b.setInternal(bValue)
    }
    override private[web] def updateInternal(f: C => C): UIO[Unit] =
      currentValue.flatMap { cValue =>
        val (aValue, bValue) = zip.unzip(f(cValue))
        a.setInternal(aValue) *> b.setInternal(bValue)
      }
    override private[web] def reRenderInternal: UIO[Unit] = a.reRenderInternal

  }

  final case class Transform[A, B](a: WidgetState[A], ab: A => B, ba: B => A) extends WidgetState[B] {

    override val renderTimeValue: B = ab(a.renderTimeValue)
    override def unsafeCurrentValue: B = ab(a.unsafeCurrentValue)

    override private[web] def setInternal(value: B): UIO[Unit] = a.setInternal(ba(value))
    override private[web] def updateInternal(f: B => B): UIO[Unit] = a.updateInternal { aValue => ba(f(ab(aValue))) }
    override private[web] def reRenderInternal: UIO[Unit] = a.reRenderInternal

  }

}
