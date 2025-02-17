package oxygen.meta.example

import oxygen.meta.*
import scala.quoted.*

trait JsonEncoder[A] {
  def encode(a: A): JsonAST
  def showInObject(a: A): Boolean = true
}
object JsonEncoder extends K0.Derivable[JsonEncoder] {

  given JsonEncoder[String] = JsonAST.JString(_)
  given JsonEncoder[Double] = JsonAST.JNumber(_)
  given JsonEncoder[Boolean] = JsonAST.JBoolean(_)
  given JsonEncoder[Int] = JsonAST.JNumber(_)

  given [A: {JsonEncoder as enc}] => JsonEncoder[Seq[A]] =
    seq => JsonAST.JArray(seq.map(enc.encode))

  given [K: {KeyEncoder as kEnc}, V: {JsonEncoder as vEnc}] => JsonEncoder[Map[K, V]] =
    map => JsonAST.JObject(map.map { case (k, v) => kEnc.encode(k) -> vEnc.encode(v) })

  given [A: {JsonEncoder as enc}] => JsonEncoder[Option[A]] =
    new JsonEncoder[Option[A]] {
      override def encode(a: Option[A]): JsonAST = a match
        case Some(value) => enc.encode(value)
        case None        => JsonAST.JNull
      override def showInObject(a: Option[A]): Boolean = a.nonEmpty
    }

  // =====| Derived |=====

  override protected def internalDeriveProduct[Q <: Quotes, A](k0: K0[Q])(g: k0.ProductGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[JsonEncoder[A]] =
    g.builders.instanceFromLazyTypeClasses[JsonEncoder] { tcs =>

      object macros {

        @scala.annotation.nowarn("msg=unused import")
        def encode(a: Expr[A]): Expr[JsonAST] = {
          val tmp: Seq[Expr[Option[(String, JsonAST)]]] =
            g.builders.mapToSeq {
              [i] =>
                (field: g.Field[i]) =>
                  import field.given

                  val fieldRename: Option[Expr[String]] = field.optionalAnnotation[fieldName].map { e => '{ $e.name } }

                  val label: Expr[String] = fieldRename.getOrElse(Expr(field.name))
                  val tc: Expr[JsonEncoder[i]] = field.typeClassInstance(tcs)
                  val value: Expr[i] = field.get(a)

                  '{
                    Option.when($tc.showInObject($value)) {
                      $label -> $tc.encode($value)
                    }
                }
            }

          '{ JsonAST.JObject(${ Expr.ofSeq(tmp) }.flatten*) }
        }

      }

      '{
        new JsonEncoder[A] {
          override def encode(a: A): JsonAST = ${ macros.encode('a) }
        }
      }
    }

  override protected def internalDeriveSum[Q <: Quotes, A](k0: K0[Q])(g: k0.SumGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[JsonEncoder[A]] =
    g.builders.instanceFromLazyTypeClasses[JsonEncoder] {
      [i <: A] =>
        (pg: k0.ProductGeneric[i]) =>
          import pg.given
          internalDeriveProduct(k0)(pg)

    } { tcs =>

      object macros {

        @scala.annotation.nowarn("msg=unused import")
        def encode(a: Expr[A]): Expr[JsonAST] =
          g.builders.matchOnInstance(a) {
            [i <: A] =>
              (kase: g.Case[i], value: Expr[i]) =>
                import kase.given

                val tc: Expr[JsonEncoder[i]] = kase.typeClassInstance(tcs)

                val name: Expr[String] =
                  kase.optionalAnnotation[caseName] match
                    case Some(caseName) => '{ $caseName.name }
                    case None           => Expr(kase.name)

                '{
                  JsonAST.JObject($name -> $tc.encode($value))
              }
          }

      }

      '{
        new JsonEncoder[A] {
          override def encode(a: A): JsonAST = ${ macros.encode('a) }
        }
      }
    }

  inline def derived[A]: JsonEncoder[A] = ${ derivedImpl[A] }

}
