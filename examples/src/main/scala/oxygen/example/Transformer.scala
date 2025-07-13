package oxygen.example

import oxygen.core.syntax.functor.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

trait Transformer[From, To] {
  def transform(from: From): To
}
object Transformer extends TransformerLowPriority.LowPriority1 {

  given id: [A] => Transformer[A, A] = identity(_)
  given idSome: [A] => Transformer[A, Option[A]] = _.some

  @scala.annotation.nowarn
  private def derivedImpl[From: Type, To: Type](using Quotes): Expr[Transformer[From, To]] = {

    def transformImpl(from: Expr[From]): Expr[To] =
      '{ ??? }

    val res: Expr[Transformer[From, To]] =
      '{
        new Transformer[From, To] {
          override def transform(from: From): To = ${ transformImpl('from) }
        }
      }

    report.info(
      s""" 
         |=====| ${TypeRepr.of[From].widen.showAnsiCode} -> ${TypeRepr.of[To].widen.showAnsiCode}|=====
         |
         |${res.showAnsiCode}
         | """.stripMargin,
    )

    res
  }

  inline def derived[A, B]: Transformer[A, B] = ${ derivedImpl[A, B] }

}

object TransformerLowPriority {

  trait LowPriority1 {

    given seq: [S1[_]: SeqOps as seqOps1, S2[_]: SeqOps as seqOps2, A, B] => (ab: Transformer[A, B]) => Transformer[S1[A], S2[B]] =
      _.map(ab.transform).into[S2]

  }

}

extension [A](self: A)
  def transformTo[B](using transformer: Transformer[A, B]): B =
    transformer.transform(self)
