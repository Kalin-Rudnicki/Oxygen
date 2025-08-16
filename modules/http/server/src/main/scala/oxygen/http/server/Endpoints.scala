package oxygen.http.server

import oxygen.http.model.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.zio.metrics.*
import scala.annotation.tailrec
import scala.quoted.*
import zio.*

final case class Endpoints(endpoints: Growable[Endpoint]) {

  def ++(that: Endpoints): Endpoints = Endpoints(this.endpoints ++ that.endpoints)

  def finish: Endpoints.Finalized = Endpoints.Finalized.ArraySeqScan(endpoints.toArraySeq)

}
object Endpoints {

  def flatten(endpoints: Endpoints*): Endpoints =
    Endpoints(Growable.many(endpoints).flatMap(_.endpoints))

  val empty: Endpoints = Endpoints(Growable.empty)

  trait Finalized {
    def eval(request: RequestContext): URIO[Scope, HttpResponse]
  }
  object Finalized {

    final case class ArraySeqScan(endpoints: ArraySeq[Endpoint]) extends Finalized {

      override def eval(request: RequestContext): URIO[Scope, HttpResponse] = {
        @tailrec
        def loop(idx: Int): URIO[Scope, HttpResponse] =
          if (idx < endpoints.length) {
            val endpoint = endpoints(idx)
            endpoint.run(request) match {
              case Some(responseEffect) => responseEffect @@ HttpServerMetrics.endpointDuration.tagged(endpoint.metricLabels).toAspect
              case None                 => loop(idx + 1)
            }
          } else
            ZIO.succeed(HttpResponse(HttpCode.`404`))

        loop(0)
      }

    }

    // TODO (KR) : TreeScan

  }

  private final class Inst[R, E, A](
      layer: Expr[ZLayer[R, E, A]],
      deriveEndpoints: Expr[DeriveEndpoints[A]],
      tag: Expr[Tag[A]],
      _rType: Type[R],
      _eType: Type[E],
      _aType: Type[A],
  ) {

    private given rType: Type[R] = _rType
    private given eType: Type[E] = _eType
    private given aType: Type[A] = _aType

    def rTypeRepr(using Quotes): TypeRepr = rType.toTypeRepr
    def eTypeRepr(using Quotes): TypeRepr = eType.toTypeRepr

    def toZIO[R2 <: R](using Quotes): Expr[ZIO[Scope & R2, E, Endpoints]] =
      '{
        $layer.build.map { env =>
          val api: A = env.get[A](using $tag)
          $deriveEndpoints.endpoints(api)
        }
      }

  }
  private object Inst {

    def from(layer: Expr[ZLayer[?, ?, ?]])(using Quotes): Inst[?, ?, ?] = {
      type R
      type E
      type A
      val layerTypeRepr: TypeRepr = layer.toTerm.tpe.widen.dealias
      given layerType: Type[ZLayer[R, E, A]] = layerTypeRepr.asTypeOf
      val (_, _rTpe, _eTpe, _aTpe) = Type.unwrap3[ZLayer, R, E, A]
      given rType: Type[R] = _rTpe
      given eType: Type[E] = _eTpe
      given aType: Type[A] = _aTpe

      val deriveEndpoints: Expr[DeriveEndpoints[A]] = Implicits.searchRequiredIgnoreMessage[DeriveEndpoints[A]](layer.toTerm.pos)
      val tag: Expr[Tag[A]] = Implicits.searchRequiredIgnoreMessage[Tag[A]](layer.toTerm.pos)

      new Inst[R, E, A](
        layer.asExprOf[ZLayer[R, E, A]],
        deriveEndpoints,
        tag,
        rType,
        eType,
        aType,
      )
    }

  }

  // TODO (KR) : add ability to enforce types
  private def layerImpl(layerExprs: Expr[Seq[ZLayer[?, ?, ?]]])(using Quotes): Expr[ZLayer[?, ?, Endpoints]] = {
    val layers: Seq[Expr[ZLayer[?, ?, ?]]] = layerExprs match
      case Varargs(layers) => layers
      case _               => report.errorAndAbort("was not passed varargs layers")

    val insts: Seq[Inst[?, ?, ?]] = layers.map(Inst.from)

    val joinedRType: TypeRepr =
      insts.foldLeft(TypeRepr.of[Any]) { (acc, inst) => AndType.companion.apply(acc, inst.rTypeRepr) }.simplified

    val joinedEType: TypeRepr =
      insts.foldLeft(TypeRepr.of[Nothing]) { (acc, inst) => OrType.companion.apply(acc, inst.eTypeRepr) }.simplified

    type R2
    type E2
    given Type[R2] = joinedRType.asTypeOf
    given Type[E2] = joinedEType.asTypeOf

    val typedInsts: Seq[Inst[? >: R2, E2, ?]] = insts.asInstanceOf[Seq[Inst[? >: R2, E2, ?]]]

    def loop(queue: List[Inst[? >: R2, E2, ?]], acc: Growable[Expr[Endpoints]]): Expr[ZIO[R2 & Scope, E2, Endpoints]] =
      queue match {
        case head :: Nil =>
          '{
            ${ head.toZIO[R2] }.map { exprs =>
              Endpoints.flatten(${ Expr.ofSeq((acc :+ ('exprs)).to[Seq]) }*)
            }
          }
        case head :: tail =>
          '{
            ${ head.toZIO[R2] }.flatMap { exprs =>
              ${ loop(tail, acc :+ 'exprs) }
            }
          }
        case Nil =>
          '{ ZIO.succeed(Endpoints.flatten(${ Expr.ofSeq(acc.to[Seq]) }*)) }
      }

    val joinedLayer: Expr[ZLayer[R2, E2, Endpoints]] =
      '{ ZLayer.scoped[R2] { ${ loop(typedInsts.toList, Growable.empty) } } }

    joinedLayer.asExprOf[ZLayer[R2, E2, Endpoints]]
  }

  private def typedLayerImpl[R: Type, E: Type](layerExprs: Expr[Seq[ZLayer[R, E, ?]]])(using Quotes): Expr[ZLayer[R, E, Endpoints]] =
    layerImpl(layerExprs).asExprOf[ZLayer[R, E, Endpoints]]

  transparent inline def layer(inline layers: ZLayer[?, ?, ?]*): ZLayer[?, ?, Endpoints] = ${ layerImpl('layers) }

  inline def typedLayer[R, E](inline layers: ZLayer[R, E, ?]*): ZLayer[R, E, Endpoints] = ${ typedLayerImpl[R, E]('layers) }

}
