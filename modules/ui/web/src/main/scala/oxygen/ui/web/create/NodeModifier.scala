package oxygen.ui.web.create

import oxygen.ui.web.PWidget

// TODO (KR) : add types to this: NodeModifier.Const, NodeModifier.Stateless, NodeModifier.Stateful
final case class NodeModifier(
    before: Widget,
    after: Widget,
) {

  def apply[Env, Action, StateGet, StateSet <: StateGet](node: PWidget.Node[Env, Action, StateGet, StateSet]): PWidget.Node[Env, Action, StateGet, StateSet] =
    node(this)

  /**
    * node(this.before, that.before, node.children, this.after, that.after)
    */
  def >>(that: NodeModifier): NodeModifier =
    NodeModifier(
      before = fragment(this.before, that.before),
      after = fragment(this.after, that.after),
    )

  /**
    * node(that.before, this.before, node.children, that.after, this.after)
    */
  def <<(that: NodeModifier): NodeModifier =
    NodeModifier(
      before = fragment(that.before, this.before),
      after = fragment(that.after, this.after),
    )

  /**
    * node(this.before, that.before, node.children, that.after, this.after)
    */
  def <>(that: NodeModifier): NodeModifier =
    NodeModifier(
      before = fragment(this.before, that.before),
      after = fragment(that.after, this.after),
    )

  /**
    * node(that.before, this.before, node.children, this.after, that.after)
    */
  def ><(that: NodeModifier): NodeModifier =
    NodeModifier(
      before = fragment(that.before, this.before),
      after = fragment(this.after, that.after),
    )

}
object NodeModifier {

  val empty: NodeModifier =
    NodeModifier(Widget.empty, Widget.empty)

  def before(beforeMods: Widget*): NodeModifier =
    NodeModifier(fragment(beforeMods*), Widget.empty)

  def after(afterMods: Widget*): NodeModifier =
    NodeModifier(Widget.empty, fragment(afterMods*))

  def surround(beforeMods: Widget*): NodeModifier =
    NodeModifier(fragment(beforeMods*), Widget.empty)

}
