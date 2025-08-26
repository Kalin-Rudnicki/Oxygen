package oxygen.ui.web.component

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.*

object Component {

  sealed trait WithProps[-Env, +Action, -StateGet, +StateSet <: StateGet] {
    private[web] final val componentId: UUID = PlatformCompat.randomUUID()
    type Props
    protected def component(props: Props, state: StateGet): Widget[Env, Action, StateGet, StateSet]
    private[web] final def componentInternal(props: Props, state: StateGet): Widget[Env, Action, StateGet, StateSet] = component(props, state)
    def apply(props: Props): Widget[Env, Action, StateGet, StateSet]
  }
  object WithProps {

    trait Stateless[-Env, +Action] extends Component.WithProps[Env, Action, Any, Nothing] {
      override protected final def component(props: Props, state: Any): Widget[Env, Action, Any, Nothing] = component(props)
      protected def component(props: Props): Widget.Stateless[Env, Action]
      override final def apply(props: Props): Widget.Stateless[Env, Action] = Widget.ComponentWithProps(this, props)
    }

    trait StatefulReadOnly[-Env, +Action, -StateGet] extends Component.WithProps[Env, Action, StateGet, Nothing] {
      override protected def component(props: Props, state: StateGet): Widget.StatefulReadOnly[Env, Action, StateGet]
      override final def apply(props: Props): Widget.StatefulReadOnly[Env, Action, StateGet] = Widget.ComponentWithProps(this, props)
    }

    trait Stateful[-Env, +Action, State] extends Component.WithProps[Env, Action, State, State] {
      override protected def component(props: Props, state: State): Widget.Stateful[Env, Action, State]
      override final def apply(props: Props): Widget.Stateful[Env, Action, State] = Widget.ComponentWithProps(this, props)
    }

  }

  sealed trait WithoutProps[-Env, +Action, -StateGet, +StateSet <: StateGet] {
    private[web] final val componentId: UUID = PlatformCompat.randomUUID()
    protected def component(state: StateGet): Widget[Env, Action, StateGet, StateSet]
    private[web] final def componentInternal(state: StateGet): Widget[Env, Action, StateGet, StateSet] = component(state)
    def apply(): Widget[Env, Action, StateGet, StateSet]
  }
  object WithoutProps {

    trait Stateless[-Env, +Action] extends Component.WithoutProps[Env, Action, Any, Nothing] {
      override protected final def component(state: Any): Widget[Env, Action, Any, Nothing] = component
      protected val component: Widget.Stateless[Env, Action]
      override final def apply(): Widget.Stateless[Env, Action] = Widget.ComponentWithoutProps(this)
    }

    trait StatefulReadOnly[-Env, +Action, -StateGet] extends Component.WithoutProps[Env, Action, StateGet, Nothing] {
      override protected def component(state: StateGet): Widget.StatefulReadOnly[Env, Action, StateGet]
      override final def apply(): Widget.StatefulReadOnly[Env, Action, StateGet] = Widget.ComponentWithoutProps(this)
    }

    trait Stateful[-Env, +Action, State] extends Component.WithoutProps[Env, Action, State, State] {
      override protected def component(state: State): Widget.Stateful[Env, Action, State]
      override final def apply(): Widget.Stateful[Env, Action, State] = Widget.ComponentWithoutProps(this)
    }

  }

}
