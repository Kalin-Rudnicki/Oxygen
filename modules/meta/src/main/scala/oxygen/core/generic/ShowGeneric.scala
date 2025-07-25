package oxygen.core.generic

import oxygen.meta.{*, given}
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.Annotation
import scala.collection.mutable
import scala.quoted.*

object ShowGeneric extends K0.Derivable[Show] {

  object annotation {

    sealed trait obfuscate extends Annotation derives FromExprT
    object obfuscate {

      final case class map(char: Char) extends obfuscate
      final case class const(str: String) extends obfuscate

    }

    final case class hide() extends Annotation derives FromExprT

    final case class fieldName(name: String) extends Annotation derives FromExprT

    final case class typeName(name: String) extends Annotation derives FromExprT

  }

  override protected def productDeriver[A](using Quotes, Type[Show], Type[A], K0.ProductGeneric[A], K0.Derivable[Show]): K0.Derivable.ProductDeriver[Show, A] =
    K0.Derivable.ProductDeriver.withInstances[Show, A] { instances =>
      new K0.Derivable.ProductDeriver.Split[Show, A] {

        private def makeWriteTo(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] =
          generic.mapChildren
            .map[Option[Growable[StringExpr]]] { [b] => (_, _) ?=> (field: generic.Field[b]) =>
              val valueExpr: Option[StringExpr] =
                (field.annotations.optionalOfValue[ShowGeneric.annotation.obfuscate], field.annotations.optionalOfValue[ShowGeneric.annotation.hide]) match {
                  case (None, None) =>
                    StringExpr.StrBuilder { builder => '{ ${ field.getExpr(instances) }.writeTo($builder, ${ field.fromParent(value) }) } }.some
                  case (Some(ShowGeneric.annotation.obfuscate.map(char)), None) =>
                    StringExpr.StrBuilder { builder => '{ $builder.append(${ field.getExpr(instances) }.show(${ field.fromParent(value) }).map { _ => ${ Expr(char) } }) } }.some
                  case (Some(ShowGeneric.annotation.obfuscate.const(str)), None) =>
                    StringExpr.const(str).some
                  case (None, Some(_))    => None
                  case (Some(_), Some(_)) => report.errorAndAbort("You can not both hide and obfuscate a field")
                }

              valueExpr.map { value =>
                Growable(
                  StringExpr.const(field.annotations.optionalOfValue[annotation.fieldName].fold(field.name)(_.name)),
                  StringExpr.const(" = "),
                  value,
                )
              }
            }
            .flatMap(identity)
            .surround(
              Growable.single(StringExpr.const(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name) + "(")),
              Growable.single(StringExpr.const(", ")),
              Growable.single(StringExpr.const(")")),
            )
            .flatten
            .exprMkStringTo(builder)

        override def deriveCaseClass(generic: K0.ProductGeneric.CaseClassGeneric[A]): Expr[Show[A]] =
          '{
            new Show.PreferBuffer[A] {
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit = ${ makeWriteTo('builder, 'value) }
            }
          }

        override def deriveAnyVal[B: Type](generic: K0.ProductGeneric.AnyValGeneric[A, B]): Expr[Show[A]] =
          '{
            new Show[A] {
              override def show(value: A): String =
                ${ generic.field.getExpr(instances) }.show(${ generic.anyVal.unwrap('value) })
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit =
                ${ generic.field.getExpr(instances) }.writeTo(builder, ${ generic.anyVal.unwrap('value) })
            }
          }

        override def deriveCaseObject(generic: K0.ProductGeneric.CaseObjectGeneric[A]): Expr[Show[A]] =
          '{
            new Show[A] {
              override def show(value: A): String = ${ Expr(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name)) }
            }
          }

      }
    }

  override protected def sumDeriver[A](using Quotes, Type[Show], Type[A], K0.SumGeneric[A], K0.Derivable[Show]): K0.Derivable.SumDeriver[Show, A] =
    K0.Derivable.SumDeriver.withInstances[Show, A] { instances =>
      new K0.Derivable.SumDeriver[Show, A] {

        private def showChild(builder: Expr[mutable.StringBuilder], value: Expr[A]): Expr[Unit] =
          generic.matcher.instance[Unit](value) { [b <: A] => (_, _) ?=> (kase: generic.Case[b]) =>
            kase.caseExtractor.withRHS { bValue =>
              '{ ${ kase.getExpr(instances) }.writeTo($builder, $bValue) }
            }
          }

        override def derive: Expr[Show[A]] =
          '{
            new Show.PreferBuffer[A] {
              override def writeTo(builder: mutable.StringBuilder, value: A): Unit = {
                builder.append(${ Expr(generic.annotations.optionalOfValue[annotation.typeName].fold(generic.label)(_.name) + ".") })
                ${ showChild('builder, 'value) }
              }
            }
          }

      }
    }

  inline def derived[A]: Show[A] = ${ derivedImpl[A] }

}
