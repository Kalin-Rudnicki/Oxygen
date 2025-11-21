package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

object SectionWithHeader extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      header: SectionHeader.Decorator,
      section: Section.Decorator,
      postHeaderSpacing: String,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        header = SectionHeader.Decorator.empty,
        section = Section.Decorator.empty,
        postHeaderSpacing = "0",
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    final lazy val section1: Decorator =
      make("section1") { props => props.copy(header = props.header >> SectionHeader.Decorator.section1, section = props.section >> Section.Decorator.section1, postHeaderSpacing = S.spacing._5) }

    final lazy val section2: Decorator =
      make("section2") { props => props.copy(header = props.header >> SectionHeader.Decorator.section2, section = props.section >> Section.Decorator.section2, postHeaderSpacing = S.spacing._5) }

    final lazy val section3: Decorator =
      make("section3") { props => props.copy(header = props.header >> SectionHeader.Decorator.section3, section = props.section >> Section.Decorator.section3, postHeaderSpacing = S.spacing._3) }

    final def header(f: SectionHeader.Decorator => SectionHeader.Decorator): Decorator = {
      val dec = f(SectionHeader.Decorator.empty)
      make(s"header { ${dec.name} }") { current => current.copy(header = current.header >> dec) }
    }

    final def section(f: Section.Decorator => Section.Decorator): Decorator = {
      val dec = f(Section.Decorator.empty)
      make(s"section { ${dec.name} }") { current => current.copy(section = current.section >> dec) }
    }

    final def headerColor(c: String): Decorator = header(_.color(c))
    final lazy val primary: Decorator = header(_.primary)
    final lazy val positive: Decorator = header(_.positive)
    final lazy val negative: Decorator = header(_.negative)
    final lazy val alert: Decorator = header(_.alert)
    final lazy val informational: Decorator = header(_.informational)
    final lazy val brandPrimary1: Decorator = header(_.brandPrimary1)
    final lazy val brandPrimary2: Decorator = header(_.brandPrimary2)

  }

  final class Decorator private[SectionWithHeader] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(headerText: String, decorator: Decorator): Node = {
    val props = decorator.computed

    Section(props.section)(
      SectionHeader(headerText, props.header.mod(marginBottom := props.postHeaderSpacing)),
    )
  }

  def section1(headerText: String, decorator: Decorator => Decorator = identity): Node =
    apply(headerText, decorator(Decorator.section1))

  def section2(headerText: String, decorator: Decorator => Decorator = identity): Node =
    apply(headerText, decorator(Decorator.section2))

  def section3(headerText: String, decorator: Decorator => Decorator = identity): Node =
    apply(headerText, decorator(Decorator.section3))

}
