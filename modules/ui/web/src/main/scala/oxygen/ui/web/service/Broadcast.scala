package oxygen.ui.web.service

import org.scalajs.dom.BroadcastChannel as BroadcastChannelJs
import oxygen.schema.*
import oxygen.ui.web.style.OxygenThemes
import zio.*
import zio.stream.*

/**
  * Scope-owned typed wrapper around the browser `BroadcastChannel` API.
  *
  * - [[post]] encodes with the channel schema and posts to other browsing contexts
  * - [[subscribe]] is a ZStream of decoded messages (via an internal Hub)
  */
final class Broadcast[A](
    channel: BroadcastChannelJs,
    hub: Hub[A],
    schema: AnySchemaT[A],
) {

  def name: String = channel.name

  def post(value: A): UIO[Unit] =
    ZIO.succeed { channel.postMessage(schema.encode(value)) }

  def subscribe: UStream[A] =
    ZStream.scoped(hub.subscribe).flatMap(ZStream.fromQueue(_))

}
object Broadcast {

  /** Cross-tab color-mode channel (light / dark / system). */
  val themeChannel: String = "oxygen.color-mode"

  /** Cross-tab theme-pack channel (pack id). */
  val themePackChannel: String = "oxygen.theme-pack"

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Construction
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def makeJsScoped(channel: String): URIO[Scope, BroadcastChannelJs] =
    ZIO.attempt { new BroadcastChannelJs(channel) }.orDie.withFinalizer { c =>
      ZIO.attempt { c.close() }.ignore
    }

  def make[A](channel: String, schema: AnySchemaT[A]): URIO[Scope, Broadcast[A]] =
    for {
      js <- makeJsScoped(channel)
      hub <- Hub.unbounded[A]
      _ <- ZIO.addFinalizer { hub.shutdown }
      _ <- ZIO.succeed {
        js.onmessage = { event =>
          Unsafe.unsafely {
            Runtime.default.unsafe.run {
              schema.decode(event.data.toString) match {
                case Right(value) => hub.offer(value)
                case Left(error)  => ZIO.logWarning(s"Unable to parse value from channel '$channel': $error")
              }
            }
          }
        }
      }
    } yield new Broadcast[A](js, hub, schema)

  def plain[A: PlainTextSchema as s](channel: String): URIO[Scope, Broadcast[A]] =
    make[A](channel, s)

  def json[A: JsonSchema as s](channel: String): URIO[Scope, Broadcast[A]] =
    make[A](channel, s)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      One-shot helpers (no long-lived Scope)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Open channel → encode + post → close. Prefer [[Broadcast.post]] when you already hold a
    * scoped channel for the same name.
    */
  object post {

    private def internal[A](channel: String, value: A, schema: AnySchemaT[A]): UIO[Unit] =
      ZIO.scoped { make[A](channel, schema).flatMap(_.post(value)) }

    def plain[A: PlainTextSchema as s](channel: String, value: A): UIO[Unit] = internal[A](channel, value, s)
    def json[A: JsonSchema as s](channel: String, value: A): UIO[Unit] = internal[A](channel, value, s)

  }

  object subscribe {

    private def internal[A](channel: String, schema: AnySchemaT[A]): UStream[A] =
      ZStream.scoped { make[A](channel, schema) }.flatMap(_.subscribe)

    def plain[A: PlainTextSchema as s](channel: String): UStream[A] = internal[A](channel, s)
    def json[A: JsonSchema as s](channel: String): UStream[A] = internal[A](channel, s)

  }

  def postThemeMode(mode: ColorMode.Mode): UIO[Unit] = post.plain[ColorMode.Mode](themeChannel, mode)
  def subscribeThemeMode: UStream[ColorMode.Mode] = subscribe.plain[ColorMode.Mode](themeChannel)

  def postThemePack(pack: OxygenThemes.Pack): UIO[Unit] = post.plain[OxygenThemes.Pack](themePackChannel, pack)
  def subscribeThemePack: UStream[OxygenThemes.Pack] = subscribe.plain[OxygenThemes.Pack](themePackChannel)

}
