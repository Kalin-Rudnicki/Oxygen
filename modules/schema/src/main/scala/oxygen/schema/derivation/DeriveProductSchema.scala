package oxygen.schema.derivation

import oxygen.meta.*
// import oxygen.predef.core.*
import oxygen.schema.*
import scala.quoted.*
import zio.Chunk
import zio.json.*
import zio.json.ast.Json
import zio.json.internal.RetractReader
import zio.json.internal.Write

// FIX-PRE-MERGE (KR) : audit these
@scala.annotation.nowarn("msg=unused import")
@scala.annotation.nowarn("msg=unused implicit parameter")
@scala.annotation.nowarn("msg=unused explicit parameter")
private[schema] final class DeriveProductSchema[Q <: Quotes, A](val k0: K0[Q])(val g: k0.ProductGeneric[A], val tcs: k0.ValExpressions[JsonSchema])(using quotes: Q, tpe: Type[A]) {

  private def unsafeDecodeImpl(trace: Expr[List[JsonError]], in: Expr[RetractReader]): Expr[A] =
    '{
      ???
    }

  private def unsafeFromJsonASTImpl(trace: Expr[List[JsonError]], json: Expr[Json]): Expr[A] =
    '{
      ???
    }

  private def unsafeEncodeSimpleImpl(a: Expr[A], out: Expr[Write]): Expr[Unit] = {
    val eachField: Seq[Expr[Unit]] =
      g.builders.mapToSeq {
        [i] =>
          (field: g.Field[i]) =>
            import field.given

            val base: Expr[Unit] =
              '{
                JsonEncoder.string.unsafeEncode(${ Expr(field.name) }, None, $out)
                $out.write(':')
                ${ field.getExpr(tcs) }.jsonCodec.encoder.unsafeEncode(${ field.get(a) }, None, $out)
              }

            if (field.idx == 0)
              base
            else
              '{
                $out.write(',')
                $base
            }
      }

    '{
      $out.write('{')
      ${ Expr.block(eachField.toList, '{ $out.write('}') }) }
    }
  }

  private def unsafeEncodePrettyImpl(a: Expr[A], indent: Expr[Int], out: Expr[Write]): Expr[Unit] =
    '{
      // TODO (KR) :
      ()
    }

  private def unsafeEncodeImpl(a: Expr[A], indent: Expr[Option[Int]], out: Expr[Write]): Expr[Unit] =
    '{
      $indent match {
        case Some(indent) => ${ unsafeEncodePrettyImpl(a, 'indent, out) }
        case None         => ${ unsafeEncodeSimpleImpl(a, out) }
      }
    }

  private def toJsonASTImpl(a: Expr[A]): Expr[Either[String, Json]] =
    '{
      ???
    }

  private def makeJsonDecoder: Expr[JsonDecoder[A]] =
    '{
      new JsonDecoder[A] {

        override def unsafeDecode(trace: List[JsonError], in: RetractReader): A =
          ${ unsafeDecodeImpl('trace, 'in) }

        override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
          ${ unsafeFromJsonASTImpl('trace, 'json) }

      }
    }

  private def makeJsonEncoder: Expr[JsonEncoder[A]] =
    '{
      new JsonEncoder[A] {

        override def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit =
          ${ unsafeEncodeImpl('a, 'indent, 'out) }

        override def toJsonAST(a: A): Either[String, Json] =
          ${ toJsonASTImpl('a) }

      }
    }

  private def fields: Expr[Seq[JsonSchema.ProductSchema.Field[?]]] =
    Expr.ofSeq {
      g.builders.mapToSeq {
        [i] =>
          (field: g.Field[i]) =>
            import field.given
            '{
              JsonSchema.ProductSchema.Field(
                name = ${ Expr(field.name) },
                schema = ${ field.getExpr(tcs) },
              )
          }
      }
    }

  def makeJsonSchema: Expr[JsonSchema[A]] =
    '{
      val decoder: JsonDecoder[A] = $makeJsonDecoder
      val encoder: JsonEncoder[A] = $makeJsonEncoder
      val codec: JsonCodec[A] = JsonCodec(encoder, decoder)

      JsonSchema.ProductSchema[A](
        name = ${ Expr(g.typeRepr.show) },
        jsonCodec = codec,
        fields = Chunk.fromIterable($fields),
      )
    }

}
