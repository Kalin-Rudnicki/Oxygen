package oxygen.ui.web.create

import oxygen.predef.core.*
import oxygen.ui.web.internal.{StyleSheetElement, StyleSheetSelector}
import scala.Dynamic
import scala.language.dynamics

abstract class StyleSheetBuilder extends StyleSheetBuilder.HasClass(Nil), StyleSheetBuilder.HasBuild {

  object T extends Dynamic {
    def apply(name: String): StyleSheetSelector.RootWithTag = StyleSheetSelector.tag(name)
    def selectDynamic(name: String): StyleSheetSelector.RootWithTag = StyleSheetSelector.tag(name)
  }

  object C extends Dynamic {
    def apply(name: String): StyleSheetSelector.RootWithoutTag = StyleSheetSelector.`class`(name)
    def selectDynamic(name: String): StyleSheetSelector.RootWithoutTag = StyleSheetSelector.`class`(name)
  }

  object PC extends Dynamic {
    def apply(name: String): StyleSheetSelector.NonRoot = StyleSheetSelector.pseudoClass(name)
    def selectDynamic(name: String): StyleSheetSelector.NonRoot = StyleSheetSelector.pseudoClass(name)
  }

  // = StyleSheet.derived[_.type]
  val compiled: StyleSheet

}
object StyleSheetBuilder {

  sealed trait HasBuild {

    private val builder = Seq.newBuilder[StyleSheetElement.AppliedStyleSheet]
    given StyleSheetBuilder.MutableAdder = builder.addOne(_)
    private[web] def built: Growable[StyleSheetElement.AppliedStyleSheet] = Growable.many(builder.result())

  }

  sealed trait HasSelector {

    lazy val className: String
    lazy val classNames: Set[String]
    final lazy val selector: StyleSheetSelector.RootWithoutTag = StyleSheetSelector.`class`(className)

  }

  sealed abstract class HasClass(final val classScope: List[String]) {

    abstract class Class(name: String) extends HasClass(classScope :+ name), HasSelector, HasBuild { klass =>

      override final lazy val className: String = classScope.mkString("--")
      override final lazy val classNames: Set[String] = Set(className)

      final def optMods(mods: (klass.type => (klass.Modifier, Boolean))*): ClassAttr =
        Widget.raw.`class` {
          mods.flatMap {
            _(klass) match {
              case (mod, true) => mod.className.some
              case (_, false)  => None
            }
          }.toSet + klass.className
        }

      abstract class Modifier(name: String) extends HasSelector, HasBuild {

        override final lazy val className: String = s"${klass.className}__$name"
        override final lazy val classNames: Set[String] = Set(klass.className, className)

      }

    }

  }

  trait MutableAdder {
    def add(ass: StyleSheetElement.AppliedStyleSheet): Unit
  }
  object MutableAdder {
    val noop: MutableAdder = _ => ()
  }

}
