package oxygen.schema.derivation

import oxygen.predef.core.*
import zio.json.*
import zio.json.JsonDecoder.UnsafeJson
import zio.json.internal.*

final class DecoderCache[A](primaryName: String, decoder: JsonDecoder[A]) {

  private var mutableValue: Option[(String, A)] = None

  def decode(inField: String, trace0: List[JsonError], in: RetractReader): Unit = {
    val trace1: List[JsonError] = JsonError.ObjectAccess(inField) :: trace0

    mutableValue match {
      case Some((`inField`, _))     => throw UnsafeJson(JsonError.Message("Duplicated field") :: trace1)
      case Some((alreadyParsed, _)) => throw UnsafeJson(JsonError.Message(s"Duplicated field (alias collision: $inField, $alreadyParsed)") :: trace1)
      case None                     =>
    }

    mutableValue = (inField, decoder.unsafeDecode(trace1, in)).some
  }

  def getOrThrow(trace0: List[JsonError]): A = mutableValue match
    case Some((_, value)) => value
    case None             => decoder.unsafeDecodeMissing(JsonError.ObjectAccess(primaryName) :: trace0)

}
