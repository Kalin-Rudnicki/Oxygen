package oxygen.meta.example

import oxygen.meta.*
import oxygen.predef.core.*
import scala.quoted.*

trait JsonDecoder[A] {

  def decode(json: JsonAST): Either[JError, A]
  def onMissing: Option[A] = None

  final def map[B](f: A => B): JsonDecoder[B] = decode(_).map(f)
  final def mapDecode[B](f: A => Either[String, B]): JsonDecoder[B] = decode(_).flatMap(f(_).leftMap(JError.InvalidJson(_)))

}
object JsonDecoder extends K0.Derivable[JsonDecoder] {

  inline def apply[A](using dec: JsonDecoder[A]): JsonDecoder[A] = dec

  private def decodePartial[A: {TypeTag as tt}](f: PartialFunction[JsonAST, A]): JsonDecoder[A] = {
    case f(a) => a.asRight
    case _    => JError.InvalidJson(s"Invalid json for ${tt.prefixObject}").asLeft
  }

  given JsonDecoder[String] = decodePartial { case JsonAST.JString(value) => value }
  given JsonDecoder[Double] = decodePartial {
    case JsonAST.JNumber(value)                                  => value
    case JsonAST.JString(value) if value.toDoubleOption.nonEmpty => value.toDouble
  }
  given JsonDecoder[Boolean] = decodePartial {
    case JsonAST.JBoolean(value)                                  => value
    case JsonAST.JString(value) if value.toBooleanOption.nonEmpty => value.toBoolean
  }
  given JsonDecoder[Int] = JsonDecoder[Double].mapDecode { num =>
    if (num.isWhole && num >= Int.MinValue && num <= Int.MaxValue) num.toInt.asRight
    else s"not a valid int ($num)".asLeft
  }

  given [A: {JsonDecoder as dec, TypeTag as tt}] => JsonDecoder[Seq[A]] = {
    case JsonAST.JArray(elements) =>
      elements.zipWithIndex.traverse { case (e, i) =>
        dec.decode(e).leftMap(JError.InIndex(i, _))
      }
    case _ =>
      JError.InvalidJson(s"Invalid json for Seq[${tt.prefixObject}], expected Array").asLeft
  }

  given [K: {KeyDecoder as kDec, TypeTag as kTT}, V: {JsonDecoder as vDec, TypeTag as vTT}] => JsonDecoder[Map[K, V]] = {
    case JsonAST.JObject(fields) =>
      fields.toList
        .traverse { case (k, v) =>
          for {
            key <- kDec.decode(k).leftMap(e => JError.InvalidKey(k, e))
            value <- vDec.decode(v).leftMap(JError.InField(k, _))
          } yield (key, value)
        }
        .map(_.toMap)
    case _ =>
      JError.InvalidJson(s"Invalid json for Map[${kTT.prefixObject}, ${vTT.prefixObject}], expected Object").asLeft
  }

  given [A: {JsonDecoder as dec}] => JsonDecoder[Option[A]] =
    new JsonDecoder[Option[A]] {
      override def decode(json: JsonAST): Either[JError, Option[A]] = json match
        case JsonAST.JNull => None.asRight
        case _             => dec.decode(json).map(_.some)
      override def onMissing: Option[Option[A]] = None.some
    }

  // =====| Derived |=====

  override protected def internalDeriveProduct[Q <: Quotes, A](k0: K0[Q])(g: k0.ProductGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[JsonDecoder[A]] = {
    g.builders.instanceFromLazyTypeClasses[JsonDecoder] { tcs =>

      object macros {

        def decode(json: Expr[JsonAST]): Expr[Either[JError, A]] = '{
          $json match
            case JsonAST.JObject(fields) => ${ decodeFields('fields) }
            case _                       => JError.InvalidJson("Expected Object").asLeft
        }

        @scala.annotation.nowarn("msg=unused import")
        private def decodeFields(map: Expr[Map[String, JsonAST]]): Expr[Either[JError, A]] =
          g.builders.eitherMapToInstance[JError] {
            [i] =>
              (field: g.Field[i]) =>
                import field.given

                val fieldRename: Option[Expr[String]] = field.optionalAnnotation[fieldName].map { e => '{ $e.name } }

                val label: Expr[String] = fieldRename.getOrElse(Expr(field.name))
                val tc: Expr[JsonDecoder[i]] = field.typeClassInstance(tcs)

                '{
                  $map.get($label) match {
                    case Some(value) => $tc.decode(value).leftMap(JError.InField($label, _))
                    case None        => $tc.onMissing.toRight(JError.InField($label, JError.InvalidJson("missing")))
                  }
              }
          }

      }

      '{
        new JsonDecoder[A] {
          override def decode(json: JsonAST): Either[JError, A] = ${ macros.decode('json) }
        }
      }

    }

  }

  override protected def internalDeriveSum[Q <: Quotes, A](k0: K0[Q])(g: k0.SumGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[JsonDecoder[A]] =
    g.builders.instanceFromLazyTypeClasses[JsonDecoder] {
      [i <: A] =>
        (pg: k0.ProductGeneric[i]) =>
          import pg.given
          internalDeriveProduct(k0)(pg)

    } { tcs =>

      object macros {

        def decode(json: Expr[JsonAST]): Expr[Either[JError, A]] =
          '{
            $json match {
              case JsonAST.JObject(fields) =>
                fields.toList match {
                  case (k, v) :: Nil => ${ decodeSingleKeyObject('k, 'v) }
                  case _             => JError.InvalidJson("expected single key object").asLeft

                }
              case _ => JError.InvalidJson("expected single key object").asLeft
            }
          }

        @scala.annotation.nowarn("msg=unused import")
        private def decodeSingleKeyObject(key: Expr[String], value: Expr[JsonAST]): Expr[Either[JError, A]] =
          g.builders.matchOnInput[String, Either[JError, A]](key) {
            [i <: A] =>
              (kase: g.Case[i]) =>
                import kase.given

                val tc: Expr[JsonDecoder[i]] = kase.typeClassInstance(tcs)

                val name: Expr[String] =
                  kase.optionalAnnotation[caseName] match
                    case Some(caseName) => '{ $caseName.name }
                    case None           => Expr(kase.name)

                (
                  name,
                  '{
                    $tc.decode($value).leftMap(JError.InField($key, _))
                  },
              )
          } {
            '{
              JError
                .InvalidKey(
                  $key,
                  ${ Expr(s"expected one of: ${g.cases.map(c => s"`${c.name}`").mkString(", ")}") },
                )
                .asLeft
            }
          }

      }

      '{
        new JsonDecoder[A] {
          override def decode(json: JsonAST): Either[JError, A] = ${ macros.decode('json) }
        }
      }
    }

  inline def derived[A]: JsonDecoder[A] = ${ derivedImpl[A] }

}
