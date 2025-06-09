package oxygen.quoted

import oxygen.quoted.companion.*
import scala.quoted.*

final class Signature private (using val quotes: Quotes)(val unwrap: quotes.reflect.Signature) {
  def unwrapWithin(using newQuotes: Quotes): newQuotes.reflect.Signature = this.unwrap.asInstanceOf[newQuotes.reflect.Signature]

  /**
    * The signatures of the method parameters.
    *
    *  Each *type parameter section* is represented by a single Int corresponding
    *  to the number of type parameters in the section.
    *  Each *term parameter* is represented by a String corresponding to the fully qualified
    *  name of the parameter type.
    */
  def paramSigs: List[String | Int] = this.unwrap.paramSigs

  /** The signature of the result type */
  def resultSig: String = this.unwrap.resultSig

}
object Signature {

  def wrap(using quotes: Quotes)(unwrap: quotes.reflect.Signature): Signature = Signature(unwrap)

  def companion(using quotes: Quotes): SignatureCompanion = SignatureCompanion(using quotes)
  given Quotes => Conversion[Signature.type, SignatureCompanion] = _.companion

  /** Matches the method signature and returns its parameters and result type. */
  def unapply(x: Signature): (List[String | Int], String) = {
    given q: Quotes = x.quotes
    q.reflect.Signature.unapply(x.unwrapWithin)
  }

}
