package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}

object Dropdown {

  // TODO (KR) : have color options
  final case class Props(
      style: Style = Style.Primary,
      closeOnMouseLeave: Boolean = false,
      width: String = 25.ch,
      optionsMaxHeight: String = 100.px,
  )

  enum Style {
    case Primary
    case Positive
    case Negative
    case Alert
    case Info
    case BrandPrimary1
    case BrandPrimary2

    private[Dropdown] def modifierClasses: ClassAttr = this match
      case Style.Primary       => O.Dropdown.Primary
      case Style.Positive      => O.Dropdown.Positive
      case Style.Negative      => O.Dropdown.Negative
      case Style.Alert         => O.Dropdown.Alert
      case Style.Info          => O.Dropdown.Info
      case Style.BrandPrimary1 => O.Dropdown.BrandPrimary1
      case Style.BrandPrimary2 => O.Dropdown.BrandPrimary2

  }
  final case class State[S](
      options: Seq[S],
      selected: Option[S],
      expanded: Boolean,
  ) {

    private val lastIdx: Int = options.size - 1
    private[Dropdown] val elems: Seq[(Boolean, S, Boolean)] =
      options.zipWithIndex.map { case (value, idx) =>
        (idx == 0, value, idx == lastIdx)
      }

  }
  object State {

    def initialNone[S: StrictEnum as e]: State[S] =
      State(e.enumValues, None, false)

    def initialNone[S](options: Seq[S]): State[S] =
      State(options, None, false)

    def initialFirst[S: StrictEnum as e]: State[S] =
      State(e.enumValues, e.enumValues.head.some, false)

    def initialFirst[S](options: Seq[S]): State[S] =
      State(options, options.headOption, false)

    def initial[S: StrictEnum as e](initial: S): State[S] =
      State(e.enumValues, initial.some, false)

  }

  def apply[S](
      props: Props.type => Props = _(),
      show: S => String = (_: S).toString,
      showEmpty: String = "",
      showSetNone: Specified[String] = Specified.WasNotSpecified,
  ): WidgetS[Dropdown.State[S]] = {
    val _props: Props = props(Props)

    Widget.state[Dropdown.State[S]].fix { state =>
      val current: Option[S] = state.renderTimeValue.selected

      def makeOption(value: Option[S], text: String, isFirst: Boolean, isLast: Boolean): Widget.Const =
        div(
          O.Dropdown.Options.Option.optMods(_.Selected -> (current == value), _.First -> isFirst, _.Last -> isLast),
          onClick := state.update(_.copy(selected = value, expanded = false)),
          text,
        )

      div(
        O.Dropdown.optMods(_.Expanded -> state.renderTimeValue.expanded),
        _props.style.modifierClasses,
        Widget.when(_props.closeOnMouseLeave) { onMouseLeave := state.update(_.copy(expanded = false)).whenDiscard(state.renderTimeValue.expanded) },
        width := _props.width,
        div(
          O.Dropdown.Display,
          onClick := state.update { s => s.copy(expanded = !s.expanded) },
          current.fold(showEmpty)(show) match {
            case ""  => util.nonBreakingSpace
            case str => str
          },
        ),
        div(
          O.Dropdown.Options,
          Widget.foreach(showSetNone.toOption) { str =>
            makeOption(
              None,
              str,
              true,
              state.renderTimeValue.options.isEmpty,
            )
          },
          Widget.foreach(state.renderTimeValue.elems) { case (isFirst, opt, isLast) =>
            makeOption(
              opt.some,
              show(opt),
              isFirst && !showSetNone.isSpecified,
              isLast,
            )
          },
        ),
      )
    }
  }

}
