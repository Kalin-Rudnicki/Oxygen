package oxygen.example

import oxygen.json.Json
import oxygen.meta.*
import oxygen.meta.K0.*
import oxygen.predef.core.*
import scala.quoted.*

// Symbol, Term, Tree, TypeRepr

trait MyJsonDecoder[A] {
  def decodeJson(json: Json): Either[MyJsonDecoder.Error, A]
  def onMissing: Option[A] = None
}
object MyJsonDecoder extends Derivable[MyJsonDecoder] {

  final case class Error(message: String, path: List[String]) {
    def inField(field: String): Error = Error(message, field :: path)
    override def toString: String = s"${path.mkString("[", ", ", "]")} : $message"
  }

  given MyJsonDecoder[String] =
    new MyJsonDecoder[String] {
      override def decodeJson(json: Json): Either[Error, String] = json match
        case Json.Str(value) => value.asRight
        case _               => Error("expected string", Nil).asLeft
    }

  given MyJsonDecoder[Int] =
    new MyJsonDecoder[Int] {
      override def decodeJson(json: Json): Either[Error, Int] = json match
        case Json.Number(value) => value.toInt.asRight
        case _                  => Error("expected number", Nil).asLeft
    }

  given [A: MyJsonDecoder as aDecoder] => MyJsonDecoder[Option[A]] =
    new MyJsonDecoder[Option[A]] {
      override def decodeJson(json: Json): Either[Error, Option[A]] = json match
        case Json.Null => None.asRight
        case _         => aDecoder.decodeJson(json).map(_.some)

      override def onMissing: Option[Option[A]] =
        None.some
    }

  override protected def productDeriver[A](using Quotes, Type[MyJsonDecoder], Type[A], ProductGeneric[A], Derivable[MyJsonDecoder]): Derivable.ProductDeriver[MyJsonDecoder, A] =
    Derivable.ProductDeriver.withInstances[MyJsonDecoder, A] { instances =>
      new Derivable.ProductDeriver[MyJsonDecoder, A] {

        private def decodeJsonObjImpl(json: Expr[Json.Obj]): Expr[Either[Error, A]] =
          generic.instantiate.either[Error] {
            [b] =>
              (_, _) ?=>
                (field: generic.Field[b]) => // Expr[Either[Error, b]]
                  '{
                    $json.valueMap.get(${ Expr(field.name) }) match {
                      case Some(value) => ${ field.getExpr(instances) }.decodeJson(value).leftMap(_.inField(${ Expr(field.name) }))
                      case None        =>
                        ${ field.getExpr(instances) }.onMissing match {
                          case Some(value) => value.asRight
                          case None        => Error("missing", ${ Expr(field.name) } :: Nil).asLeft
                        }
                    }
                }
          }

        override def derive: Expr[MyJsonDecoder[A]] =
          '{
            new MyJsonDecoder[A] {
              override def decodeJson(json: Json): Either[Error, A] = json match
                case obj: Json.Obj => ${ decodeJsonObjImpl('obj) }
                case _             => Error("expected object", Nil).asLeft
            }
          }

      }
    }

  override protected def sumDeriver[A](using Quotes, Type[MyJsonDecoder], Type[A], SumGeneric[A], Derivable[MyJsonDecoder]): Derivable.SumDeriver[MyJsonDecoder, A] =
    Derivable.SumDeriver.withInstances[MyJsonDecoder, A] { instances =>
      new Derivable.SumDeriver[MyJsonDecoder, A] {

        private def decodeJsonObjImpl(key: Expr[String], value: Expr[Json]): Expr[Either[Error, A]] =
          generic.matcher.value[String, Either[Error, A]](key) {
            [b <: A] =>
              (_, _) ?=>
                (kase: generic.Case[b]) =>
                  CaseExtractor.const(Expr(kase.name)).withRHS { _ =>
                    '{ ${ kase.getExpr(instances) }.decodeJson($value) }
                }
          } {
            '{ Error("invalid key", $key :: Nil).asLeft }
          }

        override def derive: Expr[MyJsonDecoder[A]] =
          '{
            new MyJsonDecoder[A] {
              override def decodeJson(json: Json): Either[Error, A] = json match
                case Json.Obj(Contiguous((key, value))) => ${ decodeJsonObjImpl('key, 'value) }
                case Json.Obj(_)                        => Error("object does not have single key", Nil).asLeft
                case _                                  => Error("expected object", Nil).asLeft
            }
          }

      }
    }

  override inline def derived[A]: MyJsonDecoder[A] = ${ derivedImpl[A] }

}
