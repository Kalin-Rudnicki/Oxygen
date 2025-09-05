package oxygen.ui.web.service

import org.scalajs.dom.window.localStorage as JsLocalStorage
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.ui.web.UIError
import zio.*

trait LocalStorage { self =>

  protected def getInternal(key: String): UIO[Option[String]]
  protected def setInternal(key: String, value: String): UIO[Unit]
  protected def removeInternal(key: String): UIO[Unit]

  object storageKey {

    def apply(key: String): LocalStorage.StorageKey[String] =
      LocalStorage.StorageKey[String](self, key, identity, _.asRight)

    def string[A: StringCodec as codec](key: String): LocalStorage.StorageKey[A] =
      LocalStorage.StorageKey[A](self, key, codec.encoder.encode, codec.decoder.decodeSimple)

    def json[A: JsonCodec as codec](key: String): LocalStorage.StorageKey[A] =
      LocalStorage.StorageKey[A](self, key, codec.encoder.encodeJsonStringCompact, codec.decoder.decodeJsonString(_).leftMap(_.getMessage))

  }

  object find {

    def apply(key: String): UIO[Option[String]] = getInternal(key)

    def string[A: StringDecoder as dec](key: String): IO[UIError.ClientSide.InternalDefect, Option[A]] =
      find(key).flatMap {
        ZIO.foreach(_) { value => ZIO.fromEither(dec.decodeSimple(value).leftMap(UIError.ClientSide.InternalDefect.somethingWentWrong(_))) }
      }

    def json[A: JsonDecoder as dec](key: String): IO[UIError.ClientSide.InternalDefect, Option[A]] =
      find(key).flatMap {
        ZIO.foreach(_) { value => ZIO.fromEither(dec.decodeJsonString(value).leftMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(e.getMessage))) }
      }

  }

  object get {

    def apply(key: String): IO[UIError.ClientSide.InternalDefect, String] =
      getInternal(key).someOrFail(UIError.ClientSide.InternalDefect.somethingWentWrong(s"Missing required local-storage key '$key'"))

    def string[A: StringDecoder as dec](key: String): IO[UIError.ClientSide.InternalDefect, A] =
      get(key).flatMap { value =>
        ZIO.fromEither(dec.decodeSimple(value).leftMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(s"Error decoding local-storage value for key '$key': $e")))
      }

    def json[A: JsonDecoder as dec](key: String): IO[UIError.ClientSide.InternalDefect, A] =
      get(key).flatMap { value =>
        ZIO.fromEither(dec.decodeJsonString(value).leftMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(s"Error decoding local-storage value for key '$key': ${e.getMessage}")))
      }

  }

  object set {

    def apply(key: String, value: String): UIO[Unit] = setInternal(key, value)

    def string[A: StringEncoder as enc](key: String, value: A): UIO[Unit] =
      set(key, enc.encode(value))

    def json[A: JsonEncoder as enc](key: String, value: A): UIO[Unit] =
      set(key, enc.encodeJsonStringCompact(value))

  }

  object setOption {

    def apply(key: String, value: Option[String]): UIO[Unit] =
      value match
        case Some(value) => set(key, value)
        case None        => remove(key)

    def string[A: StringEncoder as enc](key: String, value: Option[A]): UIO[Unit] =
      value match
        case Some(value) => set.string(key, value)
        case None        => remove(key)

    def json[A: JsonEncoder as enc](key: String, value: Option[A]): UIO[Unit] =
      value match
        case Some(value) => set.json(key, value)
        case None        => remove(key)

  }

  final def remove(key: String): UIO[Unit] = removeInternal(key)

}
object LocalStorage {

  final class StorageKey[A](
      localStorage: LocalStorage,
      val key: String,
      encode: A => String,
      decode: String => Either[String, A],
  ) {

    def find: IO[UIError.ClientSide.InternalDefect, Option[A]] =
      localStorage.find(key).flatMap {
        ZIO.foreach(_) { value => ZIO.fromEither(decode(value).leftMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(s"Error decoding local-storage value for key '$key': $e"))) }
      }

    def get: IO[UIError.ClientSide.InternalDefect, A] =
      localStorage.get(key).flatMap { value =>
        ZIO.fromEither(decode(value).leftMap(e => UIError.ClientSide.InternalDefect.somethingWentWrong(s"Error decoding local-storage value for key '$key': $e")))
      }

    def set(value: A): UIO[Unit] =
      localStorage.set(key, encode(value))

    def setOption(value: Option[A]): UIO[Unit] =
      localStorage.setOption(key, value.map(encode))

    def remove: UIO[Unit] =
      localStorage.remove(key)

    def cached: IO[UIError.ClientSide.InternalDefect, StorageCache[A]] =
      for {
        current <- find
        cache <- Ref.make(current)
      } yield StorageCache(this, cache)

  }

  final class StorageCache[A](
      storageKey: StorageKey[A],
      cache: Ref[Option[A]],
  ) {

    val key: String = storageKey.key

    def find: UIO[Option[A]] =
      cache.get

    def get: IO[UIError.ClientSide.InternalDefect, A] =
      cache.get.someOrFail { UIError.ClientSide.InternalDefect.somethingWentWrong(s"Missing required local-storage key '${storageKey.key}'") }

    def set(value: A): UIO[Unit] =
      storageKey.set(value) *>
        cache.set(value.some)

    def setOption(value: Option[A]): UIO[Unit] =
      storageKey.setOption(value) *>
        cache.set(value)

    def remove: UIO[Unit] =
      storageKey.remove *>
        cache.set(None)

  }

  case object Live extends LocalStorage {

    override protected def getInternal(key: String): UIO[Option[String]] =
      ZIO.attempt { Option(JsLocalStorage.getItem(key)) }.orDie

    override protected def setInternal(key: String, value: String): UIO[Unit] =
      ZIO.attempt { JsLocalStorage.setItem(key, value) }.orDie

    override protected def removeInternal(key: String): UIO[Unit] =
      ZIO.attempt { JsLocalStorage.removeItem(key) }.orDie

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Layers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val live: ULayer[LocalStorage] =
    ZLayer.succeed { Live }

}
