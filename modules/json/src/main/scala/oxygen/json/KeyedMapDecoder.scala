package oxygen.json

import oxygen.core.typeclass.SeqOps
import oxygen.predef.core.*
import scala.collection.mutable
import zio.*
import zio.json.JsonDecoder
import zio.json.ast.Json

final class KeyedMapDecoder[A](options: Seq[KeyedMapDecoder.Decoder[A]]) {
  val decoder: JsonDecoder[Chunk[A]] = KeyedMapDecoder.make(options)
}
object KeyedMapDecoder {

  final case class Decoder[A](key: String, decoder: JsonDecoder[A]) {
    def map[B](f: A => B): Decoder[B] = Decoder(key, decoder.map(f))

    def mapOrFail[B](f: A => Either[String, B]): Decoder[B] = Decoder(key, decoder.mapOrFail(f))
  }
  object Decoder {
    def make[A](key: String)(using decoder: JsonDecoder[A]): Decoder[A] = Decoder(key, decoder)
  }

  // TODO (KR) : Remove this
  private given chunkSeqOps: SeqOps[Chunk] =
    new SeqOps[Chunk] {
      override def newIterator[A](self: Chunk[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Chunk[A]] = Chunk.newBuilder
      override def knownSize[A](self: Chunk[A]): Int = self.knownSize
    }

  def make[A](options: Seq[Decoder[A]]): JsonDecoder[Chunk[A]] = {
    val map: Map[String, JsonDecoder[A]] = options.map(a => (a.key, a.decoder)).toMap
    Json.Obj.decoder.mapOrFail { obj =>
      obj.fields.traverse { case (key, value) =>
        (map.get(key) match {
          case Some(decoder) => decoder.decodeJson(value.toString)
          case None          => s"Unsupported key (valid: ${map.keys.toSeq.sorted.mkString(", ")})".asLeft
        }).leftMap(e => s".$key : $e")
      }
    }
  }

}
