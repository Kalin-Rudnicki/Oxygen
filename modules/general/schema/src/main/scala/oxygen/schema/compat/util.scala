package oxygen.schema.compat

import oxygen.predef.core.*
import oxygen.schema.compiled.*

type RefPair = (from: CompiledSchemaRef, to: CompiledSchemaRef)

sealed trait FromToValues[+A] {

  final def toIndentedString(f: A => IndentedString): IndentedString = this match
    case FromToValues.Same(value)         => f(value)
    case FromToValues.Different(from, to) => IndentedString.keyValueSection("Different:")("from: " -> f(from), "to: " -> f(to))

  final def isDifferent: Boolean = this match
    case _: FromToValues.Different[?] => true
    case _: FromToValues.Same[?]      => false

}
object FromToValues {

  def apply[A](from: A, to: A): FromToValues[A] =
    if from == to then Same(from)
    else Different(from, to)

  final case class Same[+A](value: A) extends FromToValues[A]

  final case class Different[+A](from: A, to: A) extends FromToValues[A]

}

object AddedRemovedBoth {

  sealed trait Single[+SingleT, +BothT] {

    final def fold[A](singleF: SingleT => A, bothF: BothT => A): A = this match
      case Single.Added(added)     => singleF(added)
      case Single.Removed(removed) => singleF(removed)
      case Single.Both(both)       => bothF(both)

  }
  object Single {

    final case class Added[+SingleT](added: SingleT) extends Single[SingleT, Nothing]
    final case class Removed[+SingleT](removed: SingleT) extends Single[SingleT, Nothing]
    final case class Both[+BothT](both: BothT) extends Single[Nothing, BothT]
  }

  final case class Many[+SingleT, +BothT](
      added: Seq[SingleT],
      removed: Seq[SingleT],
      both: Seq[BothT],
  ) {

    def toSingles: Seq[AddedRemovedBoth.Single[SingleT, BothT]] =
      Seq(
        added.map(AddedRemovedBoth.Single.Added(_)),
        removed.map(AddedRemovedBoth.Single.Removed(_)),
        both.map(AddedRemovedBoth.Single.Both(_)),
      ).flatten

    def size: Int = added.size + removed.size + both.size

    def nonEmpty: Boolean = added.nonEmpty || removed.nonEmpty || both.nonEmpty
    def isEmpty: Boolean = !nonEmpty

    def prune[BothT2](f: BothT => Option[BothT2]): Many[SingleT, BothT2] =
      Many(added = added, removed = removed, both = both.flatMap(f))

    def toIndentedString(single: SingleT => IndentedString, both: BothT => IndentedString): IndentedString =
      IndentedString.keyValues(
        s"added (${this.added.size}): " -> this.added.map(single),
        s"removed (${this.removed.size}): " -> this.removed.map(single),
        s"both (${this.both.size}): " -> this.both.map(both),
      )

  }
  object Many {

    def fromSingles[SingleT, BothT](singles: Seq[AddedRemovedBoth.Single[SingleT, BothT]]): AddedRemovedBoth.Many[SingleT, BothT] =
      AddedRemovedBoth.Many(
        added = singles.collect { case AddedRemovedBoth.Single.Added(added) => added },
        removed = singles.collect { case AddedRemovedBoth.Single.Removed(removed) => removed },
        both = singles.collect { case AddedRemovedBoth.Single.Both(both) => both },
      )

    def fromSinglesSorted[SingleT, BothT, A](singles: Seq[AddedRemovedBoth.Single[SingleT, BothT]])(
        single: SingleT => A,
        both: BothT => A,
    )(using Ordering[A]): AddedRemovedBoth.Many[SingleT, BothT] =
      AddedRemovedBoth.Many.fromSingles(singles.sortBy(_.fold(single, both)))

    def simpleSortedSet[A: Ordering](from: Set[A], to: Set[A]): AddedRemovedBoth.Many[A, A] =
      AddedRemovedBoth.Many(
        added = (to &~ from).toSeq.sorted,
        removed = (from &~ to).toSeq.sorted,
        both = (from & to).toSeq.sorted,
      )

    def fromMaps[K: Ordering, A, B](from: Map[K, A], to: Map[K, A])(both: (A, A) => Compared[B]): Compared[AddedRemovedBoth.Many[A, B]] = {
      val zippedAndSorted: Seq[Ior[A, A]] = Ior.zippedMapIterator(from, to).toSeq.sortBy(_._1).map(_._2)
      val singles: Seq[AddedRemovedBoth.Single[A, (A, A)]] =
        zippedAndSorted.map {
          case Ior.Both(left, right) => AddedRemovedBoth.Single.Both((left, right))
          case Ior.Left(left)        => AddedRemovedBoth.Single.Removed(left)
          case Ior.Right(right)      => AddedRemovedBoth.Single.Added(right)
        }

      Compared
        .traverse(singles) {
          case added: AddedRemovedBoth.Single.Added[A]     => Compared.done(added)
          case removed: AddedRemovedBoth.Single.Removed[A] => Compared.done(removed)
          case AddedRemovedBoth.Single.Both((from, to))    => both(from, to).map(AddedRemovedBoth.Single.Both(_))
        }
        .map(AddedRemovedBoth.Many.fromSingles)
    }

    def fromSeqs[K: Ordering, A, B](from: Seq[A], to: Seq[A], key: A => K)(both: (A, A) => Compared[B]): Compared[AddedRemovedBoth.Many[A, B]] =
      fromMaps[K, A, B](from.map { a => key(a) -> a }.toMap, to.map { a => key(a) -> a }.toMap)(both)

  }

}
