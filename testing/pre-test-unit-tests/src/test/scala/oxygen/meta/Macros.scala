package oxygen.meta

import oxygen.meta2.*
import oxygen.quoted.*
import scala.quoted.*

object Macros {

  private def seqImpl[S[_]: Type, A: Type](values: Expr[Seq[A]])(using Quotes): Expr[S[A]] =
    Varargs.unapply(values).getOrElse(report.errorAndAbort("not varargs")).unsafeSeqToExprOf[S]

  inline def make[S[_], A](inline values: A*): S[A] = ${ seqImpl[S, A]('values) }

}
