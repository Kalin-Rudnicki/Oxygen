package oxygen.ui.web.create

import monocle.Setter
import oxygen.predef.core.*
import oxygen.ui.web.internal.LensUtil
import scala.annotation.publicInBinary

abstract class Decorable {

  trait PropsCompanion protected[Decorable] () {

    protected lazy val initialProps: Props
    private[Decorable] inline final def _initialProps: Props = initialProps

  }

  type Props
  val Props: PropsCompanion

  trait DecoratorBuilder0 protected[Decorable] () {

    final def name: String = genericDecorator.show

    protected val genericDecorator: GenericDecorator[Props]
    private[Decorable] inline final def _genericDecorator: GenericDecorator[Props] = genericDecorator

    protected[Decorable] final def wrap(dec: GenericDecorator[Props]): Decorator = Decorator._wrapGeneric(genericDecorator >> dec)
    protected[Decorable] final def make(name: String)(f: Props => Props): Decorator = wrap(GenericDecorator(name)(f))

    protected inline final def focusNodeModifier(name: String)(inline f1: Props => NodeModifier): FocusNodeModifier =
      FocusNodeModifier(name, LensUtil.genLens(f1))

    protected inline final def focusNodeModifier(name: String)(inline f1: Props => NodeModifier, inline f2: Props => NodeModifier): FocusNodeModifier =
      FocusNodeModifier(name, LensUtil.setBoth(LensUtil.genLens(f1), LensUtil.genLens(f2)))

    protected inline final def focusNodeModifier(name: String)(inline f1: Props => NodeModifier, inline f2: Props => NodeModifier, inline f3: Props => NodeModifier): FocusNodeModifier =
      FocusNodeModifier(name, LensUtil.setBoth(LensUtil.genLens(f1), LensUtil.setBoth(LensUtil.genLens(f2), LensUtil.genLens(f3))))

    final class FocusNodeModifier @publicInBinary private[Decorable] (name: String, lens: Setter[Props, NodeModifier]) {

      private def genericAdd(label: String, mod: NodeModifier): Decorator = make(s"$name.nodeModifier.$label")(lens.modify(_ <> mod))

      def set(mod: NodeModifier): Decorator = Decorator._wrapGeneric(GenericDecorator(s"$name.nodeModifier.set")(lens.replace(mod)))
      def add(mod: NodeModifier): Decorator = genericAdd("add", mod)
      def prepend(before: Widget*): Decorator = genericAdd("prepend", NodeModifier.before(before*))
      def append(after: Widget*): Decorator = genericAdd("append", NodeModifier.after(after*))
      def surround(before: Widget*)(after: Widget*): Decorator = genericAdd("surround", NodeModifier.surround(before*)(after*))

      def apply(after: Widget*): Decorator = append(after*)

    }

  }
  object DecoratorBuilder0 {

    trait AllowsCustom protected[Decorable] () extends DecoratorBuilder0 {

      final def custom(name: String)(f: Props => Props): Decorator = Decorator._wrapGeneric(GenericDecorator(s"custom($name)")(f))

    }

  }

  trait DecoratorBuilderType protected[Decorable] () { self: DecoratorBuilder =>

    protected[Decorable] final lazy val _computed: Props = genericDecorator.decorate(Props._initialProps)

    final def >>(that: Decorator): Decorator = Decorator._wrapGeneric { this._genericDecorator >> that._genericDecorator }
    final def <<(that: Decorator): Decorator = Decorator._wrapGeneric { this._genericDecorator << that._genericDecorator }

  }

  trait DecoratorBuilderCompanion protected[Decorable] () { self: DecoratorBuilder =>

    override protected final val genericDecorator: GenericDecorator[Props] = GenericDecorator.empty

    final val empty: Decorator = wrap(GenericDecorator.empty[Props])
    final val identity: Decorator = empty

    lazy val defaultStyling: Decorator

    final def all[S[_]: SeqRead](decorators: S[Decorator]): Decorator =
      Decorator._wrapGeneric { decorators.newIterator.foldLeft(GenericDecorator.empty[Props]) { (a, b) => a >> b._genericDecorator } }
    final def all(decorators: Decorator*): Decorator =
      all[Seq](decorators)

    protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator
    private[Decorable] final def _wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = wrapGeneric(genericDecorator)

  }

  extension (self: Decorator)
    protected inline final def computed: Props =
      self._computed

  type DecoratorBuilder <: DecoratorBuilder0

  type Decorator <: DecoratorBuilder & DecoratorBuilderType
  val Decorator: DecoratorBuilder & DecoratorBuilderCompanion

}
object Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Template
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object Template extends Decorable {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Props
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class Props(
        size: String,
    )
    object Props extends PropsCompanion {

      override protected lazy val initialProps: Props =
        Props("???")

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Decorator
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    trait DecoratorBuilder extends DecoratorBuilder0 {

      private def makeSize(size: String): Decorator = make(size)(_.copy(size = size))

      final lazy val small: Decorator = makeSize("small")
      final lazy val medium: Decorator = makeSize("medium")
      final lazy val large: Decorator = makeSize("large")

    }

    final class Decorator private[Template] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
    object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

      override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

      override lazy val defaultStyling: Decorator = empty.medium

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Widget
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    // TODO : Implement

  }

}
