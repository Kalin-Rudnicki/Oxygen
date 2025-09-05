package oxygen.ui.web.create

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.*

// TODO (KR) : revisit whether this Component concept actually makes sense
//           : what if you want to have a `Component.WithProps.Stateful[?, ?, Wrapper[S]]`?
//           : have a Component1?
object Component {

  sealed trait WithProps[-Env, +Action, -StateGet, +StateSet <: StateGet] {
    private[web] final val componentId: UUID = PlatformCompat.randomUUID()
    type Props
    protected def component(props: Props, state: StateGet): PWidget[Env, Action, StateGet, StateSet]
    private[web] final def componentInternal(props: Props, state: StateGet): PWidget[Env, Action, StateGet, StateSet] = component(props, state)
    def apply(props: Props): PWidget[Env, Action, StateGet, StateSet]
  }
  object WithProps {

    trait Stateless[-Env, +Action] extends Component.WithProps[Env, Action, Any, Nothing] {
      override protected final def component(props: Props, state: Any): PWidget[Env, Action, Any, Nothing] = component(props)
      protected def component(props: Props): WidgetEA[Env, Action]
      override final def apply(props: Props): WidgetEA[Env, Action] = PWidget.ComponentWithProps(this, props)
    }

    trait Stateful[-Env, +Action, State] extends Component.WithProps[Env, Action, State, State] {
      override protected def component(props: Props, state: State): WidgetEAS[Env, Action, State]
      override final def apply(props: Props): WidgetEAS[Env, Action, State] = PWidget.ComponentWithProps(this, props)
    }

  }

  sealed trait WithoutProps[-Env, +Action, -StateGet, +StateSet <: StateGet] {
    private[web] final val componentId: UUID = PlatformCompat.randomUUID()
    protected def component(state: StateGet): PWidget[Env, Action, StateGet, StateSet]
    private[web] final def componentInternal(state: StateGet): PWidget[Env, Action, StateGet, StateSet] = component(state)
    def apply(): PWidget[Env, Action, StateGet, StateSet]
  }
  object WithoutProps {

    trait Stateless[-Env, +Action] extends Component.WithoutProps[Env, Action, Any, Nothing] {
      override protected final def component(state: Any): WidgetEA[Env, Action] = component
      protected val component: WidgetEA[Env, Action]
      override final def apply(): WidgetEA[Env, Action] = PWidget.ComponentWithoutProps(this)
    }

    trait Stateful[-Env, +Action, State] extends Component.WithoutProps[Env, Action, State, State] {
      override protected def component(state: State): WidgetEAS[Env, Action, State]
      override final def apply(): WidgetEAS[Env, Action, State] = PWidget.ComponentWithoutProps(this)
    }

  }

}
