package oxygen.cli

import oxygen.predef.color.{*, given}

private[cli] object ValueHelpRepr {

  def inlineParts(help: Help): Option[List[ColorString]] = help.removeEmpties match
    case Help.Empty               => Some(Nil)
    case Help.Positional(name, _) => Some(List(color"[$name]".magentaFg))
    case Help.And(left, right)    => (inlineParts(left), inlineParts(right)) match
        case (Some(l), Some(r)) => Some(l ::: r)
        case _                  => None
    case Help.WithHints(annotated: Help.Base, _) => inlineParts(annotated)
    case _                                       => None

  def needsScopedChildren(help: Help): Boolean =
    !help.isEmpty && inlineParts(help).isEmpty

  def valueHints(help: Help): List[HelpHint] = help match
    case Help.WithHints(_, messages) => messages.toList
    case Help.And(left, right)       => valueHints(left) ::: valueHints(right)
    case _                           => Nil

}
