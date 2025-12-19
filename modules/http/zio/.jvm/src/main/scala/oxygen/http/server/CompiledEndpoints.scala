package oxygen.http.server

import oxygen.http.core.BodyUtil
import oxygen.http.model.internal.*
import oxygen.http.schema.*
import oxygen.predef.core.*
import oxygen.zio.syntax.log.*
import zio.*
import zio.http.{Server as _, *}

trait CompiledEndpoints {

  def handle(input: EndpointInput): ZIO[Scope, Response, Response]

  final def withRequestMiddleware(requestMiddleware: RequestMiddleware): CompiledEndpoints =
    requestMiddleware match
      case RequestMiddleware.Empty => this
      case _                       => CompiledEndpoints.WithRequestMiddleware(this, requestMiddleware)

  final def withResponseMiddleware(responseMiddleware: ResponseMiddleware): CompiledEndpoints =
    responseMiddleware match
      case ResponseMiddleware.Empty => this
      case _                        => CompiledEndpoints.WithResponseMiddleware(this, responseMiddleware)

  final def withMiddleware(
      requestMiddleware: RequestMiddleware,
      responseMiddleware: ResponseMiddleware,
  ): CompiledEndpoints =
    this.withRequestMiddleware(requestMiddleware).withResponseMiddleware(responseMiddleware)

  final def toRoutes(config: Server.Config): Routes[Any, Response] =
    Handler
      .scoped[Any] {
        Handler.fromFunctionHandler[Request] { request =>
          Handler.fromZIO[Scope, Response, Response] {
            ZIO.scope.flatMap { scope =>
              ReceivedRequest.fromRequest(request).flatMap { request =>
                CurrentRequest.ref.locallyScoped(CurrentRequest(request, scope).some) *>
                  ZIO.logInfoAnnotated("Handling http request", "method" -> request.method.name, "path" -> request.url.path.encode) *>
                  handle(EndpointInput(request, config.errorConfig))
                    .tapDefect { ZIO.logErrorCause("Unhandled defect", _) }
              }
            }
          }
        }
      }
      .toRoutes

}
object CompiledEndpoints {

  def compile(
      endpoints: AppliedEndpoints,
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): URIO[Scope, CompiledEndpoints] =
    endpointMiddleware(endpoints).map { finalEndpoints =>
      CompiledEndpoints.TreeScan.fromEndpoints(finalEndpoints).withMiddleware(requestMiddleware, responseMiddleware)
    }

  def compileEndpoints(
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): URIO[AppliedEndpoints & Scope, CompiledEndpoints] =
    ZIO.serviceWithZIO[AppliedEndpoints] { CompiledEndpoints.compile(_, requestMiddleware, responseMiddleware, endpointMiddleware) }

  def layer(
      endpoints: AppliedEndpoints,
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): TaskLayer[CompiledEndpoints] =
    ZLayer.scoped { CompiledEndpoints.compile(endpoints, requestMiddleware, responseMiddleware, endpointMiddleware) }

  def endpointLayer(
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): RLayer[AppliedEndpoints, CompiledEndpoints] =
    ZLayer.scoped { CompiledEndpoints.compileEndpoints(requestMiddleware, responseMiddleware, endpointMiddleware) }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class SeqScan private[CompiledEndpoints] (endpoints: ArraySeq[AppliedEndpoint]) extends CompiledEndpoints {

    private val endpointsLength: Int = endpoints.length

    private def loop(idx: Int, input: EndpointInput): ZIO[Scope, Response, Response] =
      if idx < endpointsLength then {
        val endpoint: AppliedEndpoint = endpoints(idx)
        endpoint.handle(input) match {
          case Some(maybeResponse) =>
            maybeResponse.flatMap {
              case Some(response) => ZIO.succeed(response)
              case None           => loop(idx + 1, input)
            }
          case None => loop(idx + 1, input)
        }
      } else //
        ZIO.fail(Response(status = Status.NotFound, body = BodyUtil.fromString(Status.NotFound.text)))

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
      loop(0, input)

  }
  object SeqScan {

    def fromEndpoints(endpoints: AppliedEndpoints): SeqScan =
      new SeqScan(endpoints.arraySeq)

  }

  final class TreeScan private[CompiledEndpoints] (tree: internal.RoutePattern) extends CompiledEndpoints {

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] = {
      val tmp = tree.compatibleEndpoints(input.request.method, input.request.fullPath)

      if tmp.matchingMethod.length == 1 then //
        tmp.matchingMethod.head.handle(input) match {
          case Some(value) => value.someOrElse(Response.status(Status.NotFound))
          case None        => ZIO.fail(Response.status(Status.NotFound))
        }
      else if tmp.matchingMethod.length > 1 then //
        new SeqScan(tmp.matchingMethod).handle(input)
      else if tmp.all.nonEmpty then {
        val matchingMethods: ArraySeq[Method] = tmp.all.flatMap(_.method).distinct.sortBy(_.name)
        if matchingMethods.nonEmpty then // TODO (KR) : improve this
          ZIO.fail(Response(status = Status.MethodNotAllowed, body = Body.fromString(s"Supported Methods: ${matchingMethods.map(_.name).mkString(", ")}")))
        else //
          ZIO.fail(Response.status(Status.NotFound))
      } else //
        ZIO.fail(Response.status(Status.NotFound))
    }

  }
  object TreeScan {

    def fromEndpoints(endpoints: AppliedEndpoints): TreeScan =
      new TreeScan(internal.RoutePattern.fromEndpoints(endpoints))

  }

  final case class WithRequestMiddleware(
      underlying: CompiledEndpoints,
      requestMiddleware: RequestMiddleware,
  ) extends CompiledEndpoints {

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
      requestMiddleware(input.request)
        .flatMap { newRequest => underlying.handle(input.copy(request = newRequest)) }

  }

  final case class WithResponseMiddleware(
      underlying: CompiledEndpoints,
      responseMiddleware: ResponseMiddleware,
  ) extends CompiledEndpoints {

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
      underlying.handle(input).flatMap { responseMiddleware.apply }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object internal {

    sealed trait RoutePattern {

      protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint]

      def toIndentedString: IndentedString

      final def compatibleEndpoints(method: Method, path: List[String]): (all: ArraySeq[AppliedEndpoint], matchingMethod: ArraySeq[AppliedEndpoint]) = {
        val all = compatibleEndpointsInternal(path).toArraySeq
        (all, all.filter(_.method.fold(true)(_ == method)))
      }

      final def toBranch: RoutePattern.Branch = this match
        case RoutePattern.Complete(endpoints) => RoutePattern.Branch(endpoints, Map.empty, None, ArraySeq.empty)
        case RoutePattern.Const(const, next)  => RoutePattern.Branch(ArraySeq.empty, Map(const -> next), None, ArraySeq.empty)
        case RoutePattern.Param(next)         => RoutePattern.Branch(ArraySeq.empty, Map.empty, next.some, ArraySeq.empty)
        case RoutePattern.RestParam(rest)     => RoutePattern.Branch(ArraySeq.empty, Map.empty, None, rest)
        case self: RoutePattern.Branch        => self
        case RoutePattern.Empty               => RoutePattern.Branch(ArraySeq.empty, Map.empty, None, ArraySeq.empty)

      final def ||(that: RoutePattern): RoutePattern = (this, that) match
        case (_, RoutePattern.Empty)                                                          => this
        case (RoutePattern.Empty, _)                                                          => that
        case (self: RoutePattern.Complete, that: RoutePattern.Complete)                       => RoutePattern.Complete(self.endpoints ++ that.endpoints)
        case (self: RoutePattern.Const, that: RoutePattern.Const) if self.const == that.const => RoutePattern.Const(self.const, self.next || that.next)
        case (self: RoutePattern.Param, that: RoutePattern.Param)                             => RoutePattern.Param(self.next || that.next)
        case (self: RoutePattern.RestParam, that: RoutePattern.RestParam)                     => RoutePattern.RestParam(self.endpoints ++ that.endpoints)
        case _                                                                                => this.toBranch.merge(that.toBranch)

    }
    object RoutePattern {

      final case class Complete(endpoints: ArraySeq[AppliedEndpoint]) extends RoutePattern {

        private val grow: Growable[AppliedEndpoint] = Growable.WrappedArraySeq(endpoints)
        override protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint] =
          if path.isEmpty then grow
          else Growable.empty

        override def toIndentedString: IndentedString =
          endpoints.map { e => s"- ${e.method.fold("[ANY]")(_.toString)} ${e.fullName}" }

      }

      final case class Const(const: String, next: RoutePattern) extends RoutePattern {

        override protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint] = path match
          case `const` :: rest => next.compatibleEndpointsInternal(rest)
          case _               => Growable.empty

        override def toIndentedString: IndentedString =
          IndentedString.section(const + " :")(next.toIndentedString)

      }

      final case class Param(next: RoutePattern) extends RoutePattern {

        override protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint] = path match
          case _ :: rest => next.compatibleEndpointsInternal(rest)
          case _         => Growable.empty

        override def toIndentedString: IndentedString =
          IndentedString.section("* :")(next.toIndentedString)

      }

      final case class RestParam(endpoints: ArraySeq[AppliedEndpoint]) extends RoutePattern {

        private val grow: Growable[AppliedEndpoint] = Growable.WrappedArraySeq(endpoints)
        override protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint] = grow

        override def toIndentedString: IndentedString =
          IndentedString.section("** :")(endpoints.map { e => s"- ${e.method.fold("[ANY]")(_.toString)} ${e.fullName}" })

      }

      final case class Branch(
          complete: ArraySeq[AppliedEndpoint],
          const: Map[String, RoutePattern],
          param: Option[RoutePattern],
          rest: ArraySeq[AppliedEndpoint],
      ) extends RoutePattern {

        private val fromRest: Growable[AppliedEndpoint] = Growable.WrappedArraySeq(rest)
        private val completeAndRest: Growable[AppliedEndpoint] = Growable.WrappedArraySeq(complete ++ rest)

        def merge(that: Branch): Branch =
          Branch(
            complete = this.complete ++ that.complete,
            const = Ior.mergeMap(this.const, that.const)(_ || _),
            param = (this.param, that.param) match {
              case (Some(self), Some(that)) => (self || that).some
              case (res @ Some(_), None)    => res
              case (None, res @ Some(_))    => res
              case (None, None)             => None
            },
            rest = this.rest ++ that.rest,
          )

        override protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint] =
          path match {
            case head :: tail =>
              val fromConst: Growable[AppliedEndpoint] = const.get(head) match
                case Some(const) => const.compatibleEndpointsInternal(tail)
                case _           => Growable.empty
              val fromParam: Growable[AppliedEndpoint] = param match
                case Some(param) => param.compatibleEndpointsInternal(tail)
                case None        => Growable.empty

              fromConst ++ fromParam ++ fromRest
            case Nil =>
              completeAndRest
          }

        override def toIndentedString: IndentedString =
          IndentedString.inline(
            if complete.nonEmpty then Complete(complete).toIndentedString
            else IndentedString.inline(),
            const.toSeq.sortBy(_._1).map(Const(_, _).toIndentedString),
            param.map(Param(_).toIndentedString),
            if rest.nonEmpty then RestParam(rest).toIndentedString
            else IndentedString.inline(),
          )

      }

      case object Empty extends RoutePattern {

        override protected def compatibleEndpointsInternal(path: List[String]): Growable[AppliedEndpoint] = Growable.empty

        override def toIndentedString: IndentedString = "<empty>"

      }

      private def fromEndpoint(endpoint: AppliedEndpoint, p: RequestPathsSchema): RoutePattern = {
        def loop(pathQueue: List[RequestPathsSchema.Single]): RoutePattern =
          pathQueue match
            case RequestPathsSchema.Const(const) :: tail         => RoutePattern.Const(const, loop(tail))
            case RequestPathsSchema.SingleParam(_, _, _) :: tail => RoutePattern.Param(loop(tail))
            case Nil if p.rest.isEmpty                           => RoutePattern.Complete(ArraySeq(endpoint))
            case Nil                                             => RoutePattern.RestParam(ArraySeq(endpoint))

        loop(p.singles.toList)
      }

      def fromEndpoint(endpoint: AppliedEndpoint): RoutePattern =
        endpoint.schema.requestSchema.paths.map { fromEndpoint(endpoint, _) }.reduceLeft { _ || _ }

      def fromEndpoints(endpoints: AppliedEndpoints): RoutePattern =
        endpoints.endpoints.foldLeft(RoutePattern.Empty: RoutePattern) { (acc, endpoint) => acc || fromEndpoint(endpoint) }

    }

  }

}
