package oxygen.example

import oxygen.core.syntax.functor.*
import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

trait Transformer[A, B] {
  def transform(a: A): B
}
object Transformer extends TransformerLowPriority.LowPriority1 {

  given id: [A] => Transformer[A, A] = identity(_)
  given idSome: [A] => Transformer[A, Option[A]] = _.some

  @scala.annotation.nowarn
  private def derivedImpl[From: Type, To: Type](using Quotes): Expr[Transformer[From, To]] = {
    val fromGen: ProductGeneric[From] = ProductGeneric.of[From]
    val toGen: ProductGeneric[To] = ProductGeneric.of[To]

    val fromFieldMap: Map[String, fromGen.Field[?]] = fromGen.fields.map { a => (a.name, a) }.toMap

    def transformField[_From: Type, _To: Type](fromField: fromGen.Field[_From], toField: toGen.Field[_To])(expr: Expr[_From]): Expr[_To] = {
      val transformer: Expr[Transformer[_From, _To]] =
        Expr.summon[Transformer[_From, _To]].getOrElse(report.errorAndAbort(s"No transformer for ${TypeRepr.of[_From].showAnsiCode} -> ${TypeRepr.of[_To].showAnsiCode}", toField.pos))

      '{ $transformer.transform($expr) }
    }

    def transformParent(): Any =
      toGen.fields.map { (toField0: toGen.Field[?]) =>
        type _From
        type _To
        val toField: toGen.Field[_To] = toField0.typedAs[_To]
        given Type[_To] = toField.tpe

        val fromField: fromGen.Field[_From] =
          fromFieldMap
            .getOrElse(
              toField.name,
              report.errorAndAbort(s"Unable to transform ${TypeRepr.of[From].widen.showAnsiCode} -> ${TypeRepr.of[To].widen.showAnsiCode}\nmissing field: ${toField.name}", toField.pos),
            )
            .typedAs[_From]
        given Type[_From] = fromField.tpe

        ()
      }

    val res: Expr[Transformer[From, To]] =
      '{
        new Transformer[From, To] {
          override def transform(a: From): To = ???
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
