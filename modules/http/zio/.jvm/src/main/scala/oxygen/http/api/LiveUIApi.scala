package oxygen.http.api

import java.time.Instant
import oxygen.http.model.*
import oxygen.json.{jsonFlatten, Json, JsonEncoder}
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.schema.JsonSchema.oxygenTime.duration
import scala.annotation.experimental
import zio.*

@experimental
sealed trait LiveUIApi extends UIApi {

  protected val config: LiveUIApi.Config

  override final def index(): UIO[Redirect] = ZIO.succeed { Redirect.path(config.indexRedirect) }

}
object LiveUIApi {

  final case class Config(
      indexRedirect: String = "/page",
      scriptPath: String,
      initialTitle: String,
      favicon: PageHtmlResponse.FaviconConfig,
  ) derives JsonSchema

  final case class ConfigDynamicCustomUIConfig(
      @jsonFlatten config: Config,
      cacheTTL: Nullable[Duration],
  ) derives JsonSchema

  final case class ConfigStaticCustomUIConfig[A](
      @jsonFlatten config: Config,
      customUIConfig: A,
  )
  object ConfigStaticCustomUIConfig {
    inline given [A] => JsonSchema[ConfigStaticCustomUIConfig[A]] = JsonSchema.derived
  }

  object layer {

    def withoutCustomUIConfig: URLayer[LiveUIApi.Config, UIApi] =
      ZLayer {
        for {
          config <- ZIO.service[LiveUIApi.Config]
          htmlResponse: PageHtmlResponse = makeResponse(config)
        } yield LiveUIApi.Static(config, htmlResponse)
      }

    object withCustomUIConfig {

      object static {

        def typed[A: {JsonEncoder, Tag}]: URLayer[LiveUIApi.ConfigStaticCustomUIConfig[A], UIApi] =
          ZLayer {
            for {
              typedConfig <- ZIO.service[LiveUIApi.ConfigStaticCustomUIConfig[A]]
              config = typedConfig.config
              customUIConfig: A = typedConfig.customUIConfig
              htmlResponse: PageHtmlResponse = makeResponse[A](config, customUIConfig)
            } yield LiveUIApi.Static(config, htmlResponse)
          }

        def rawJson: URLayer[LiveUIApi.ConfigStaticCustomUIConfig[Json], UIApi] = typed[Json]

      }

      /** Calculate [[A]] once - during layer build. */
      def calculated[R, E, A: JsonEncoder](buildCustomUIConfig: ZIO[R, E, A]): ZLayer[R & LiveUIApi.Config, E, UIApi] =
        ZLayer {
          for {
            config <- ZIO.service[LiveUIApi.Config]
            customUIConfig: A <- buildCustomUIConfig
            htmlResponse: PageHtmlResponse = makeResponse[A](config, customUIConfig)
          } yield LiveUIApi.Static(config, htmlResponse)
        }

      /** Calculate [[A]] on every HTTP route evaluation. */
      def dynamic[R, A: JsonEncoder](buildCustomUIConfig: RIO[R, A]): RLayer[R & LiveUIApi.ConfigDynamicCustomUIConfig, UIApi] =
        ZLayer {
          for {
            configDynamic <- ZIO.service[LiveUIApi.ConfigDynamicCustomUIConfig]
            config = configDynamic.config
            env <- ZIO.environment[R]
            buildCustomUIConfigEffect: UIO[A] = buildCustomUIConfig.orDie.provideEnvironment(env)
            api <- configDynamic.cacheTTL.unwrap match
              case Some(cacheTTL) => Ref.make(Option.empty[(Instant, PageHtmlResponse)]).map(LiveUIApi.DynamicTTL[A](config, cacheTTL, _, buildCustomUIConfigEffect))
              case None           => Ref.make(Option.empty[(A, PageHtmlResponse)]).map(LiveUIApi.DynamicZeroTTL[A](config, _, buildCustomUIConfigEffect))
          } yield api
        }

    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Static(
      config: LiveUIApi.Config,
      htmlResponse: PageHtmlResponse,
  ) extends LiveUIApi {

    override def getPage(rest: List[String]): UIO[PageHtmlResponse] = ZIO.succeed(htmlResponse)

  }

  final case class DynamicZeroTTL[A: JsonEncoder](
      config: Config,
      cacheRef: Ref[Option[(A, PageHtmlResponse)]],
      buildCustomUIConfig: UIO[A],
  ) extends LiveUIApi {

    override def getPage(rest: List[String]): UIO[PageHtmlResponse] =
      buildCustomUIConfig.flatMap { evalValue =>
        cacheRef.get.flatMap {
          case Some((`evalValue`, response)) => ZIO.succeed(response)
          case _                             =>
            val response: PageHtmlResponse = makeResponse[A](config, evalValue)
            cacheRef.set((evalValue, response).some).as(response)
        }
      }

  }

  final case class DynamicTTL[A: JsonEncoder](
      config: Config,
      cacheTTL: Duration,
      cacheRef: Ref[Option[(Instant, PageHtmlResponse)]],
      buildCustomUIConfig: UIO[A],
  ) extends LiveUIApi {

    override def getPage(rest: List[String]): UIO[PageHtmlResponse] =
      Clock.instant.flatMap { now =>
        cacheRef.get.flatMap {
          case Some((expiresAt, response)) if now.isBefore(expiresAt) => ZIO.succeed(response)
          case _                                                      =>
            for {
              customUIConfig <- buildCustomUIConfig
              response = makeResponse[A](config, customUIConfig)
              _ <- cacheRef.set((now.plus(cacheTTL), response).some)
            } yield response
        }
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def makeResponse(config: LiveUIApi.Config): PageHtmlResponse =
    PageHtmlResponse.makeWithoutConfig(config.initialTitle, config.scriptPath, config.favicon)

  private def makeResponse[A: JsonEncoder](config: LiveUIApi.Config, customUIConfig: A): PageHtmlResponse =
    PageHtmlResponse.makeWithConfig[A](config.initialTitle, config.scriptPath, config.favicon)(customUIConfig)

}
