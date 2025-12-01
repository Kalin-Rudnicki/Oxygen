package oxygen.core.typeclass

import oxygen.core.RightProjection
import oxygen.core.syntax.either.*
import oxygen.core.syntax.option.*
import scala.collection.immutable.ArraySeq

trait Traverse[F[_], G[_]] {

  def traverse[A, B](self: F[A])(f: A => G[B]): G[F[B]]

  final def sequence[A](self: F[G[A]]): G[F[A]] = traverse(self)(identity)

}
object Traverse extends TraverseLowPriority.LowPriority1 {

  inline def apply[F[_], G[_]](using ev: Traverse[F, G]): ev.type = ev

  def traverseArraySeq[A, B](arraySeq: ArraySeq[A])(f: A => Option[B]): Option[ArraySeq[B]] =
    Traverse.EarlyReturn.save {
      arraySeq.map {
        f(_) match {
          case Some(value) => value
          case None        => throw Traverse.EarlyReturn(None)
        }
      }.some
    }

  def traverseArraySeq[A, E, B](arraySeq: ArraySeq[A])(f: A => Either[E, B]): Either[E, ArraySeq[B]] =
    Traverse.EarlyReturn.save {
      arraySeq
        .map {
          f(_) match {
            case Right(value)   => value
            case left @ Left(_) => throw Traverse.EarlyReturn(left)
          }
        }
        .asRight[E]
    }

  // =====|  |=====

  private[typeclass] final case class EarlyReturn(value: Any) extends Throwable
  private[typeclass] object EarlyReturn {

    def save[A](thunk: => A): A =
      try {
        thunk
      } catch {
        case EarlyReturn(value) => value.asInstanceOf[A]
      }

  }

}

object TraverseLowPriority {

  trait LowPriority1 {

    given functorOption: [F[_]] => (functor: Functor[F]) => Traverse[F, Option] =
      new Traverse[F, Option] {

        // Note: This could also be done by adding a `var allSuccess: Boolean`, mapping `None => null`, and checking `allSuccess` at the end.
        //     : The current implementation was chosen for sake of efficiency.
        override def traverse[A, B](self: F[A])(f: A => Option[B]): Option[F[B]] =
          Traverse.EarlyReturn.save {
            functor
              .map(self) {
                f(_) match {
                  case Some(value) => value
                  case None        => throw Traverse.EarlyReturn(None)
                }
              }
              .some
          }

      }

    // Note: This could also be done by adding a `var left: Option[Left]`, mapping `None => null`, and checking `left` at the end.
    //     : The current implementation was chosen for sake of efficiency.
    given functorEither: [F[_], Left] => (functor: Functor[F]) => Traverse[F, RightProjection[Left]] =
      new Traverse[F, RightProjection[Left]] {

        override def traverse[A, B](self: F[A])(f: A => Either[Left, B]): Either[Left, F[B]] =
          Traverse.EarlyReturn.save {
            functor
              .map(self) {
                f(_) match {
                  case Right(value)   => value
                  case left @ Left(_) => throw Traverse.EarlyReturn(left)
                }
              }
              .asRight[Left]
          }

      }

    given arraySeqOption: Traverse[ArraySeq, Option] =
      new Traverse[ArraySeq, Option] {

        // Note: This could also be done by adding a `var allSuccess: Boolean`, mapping `None => null`, and checking `allSuccess` at the end.
        //     : The current implementation was chosen for sake of efficiency.
        override def traverse[A, B](self: ArraySeq[A])(f: A => Option[B]): Option[ArraySeq[B]] =
          Traverse.EarlyReturn.save {
            self.map {
              f(_) match {
                case Some(value) => value
                case None        => throw Traverse.EarlyReturn(None)
              }
            }.some
          }

      }

    given arraySeqEither: [Left] => Traverse[ArraySeq, RightProjection[Left]] =
      new Traverse[ArraySeq, RightProjection[Left]] {

        override def traverse[A, B](self: ArraySeq[A])(f: A => Either[Left, B]): Either[Left, ArraySeq[B]] =
          Traverse.EarlyReturn.save {
            self
              .map {
                f(_) match {
                  case Right(value)   => value
                  case left @ Left(_) => throw Traverse.EarlyReturn(left)
                }
              }
              .asRight[Left]
          }

      }

  }

}
