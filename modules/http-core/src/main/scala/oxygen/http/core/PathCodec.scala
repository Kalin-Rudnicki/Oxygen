package oxygen.http.core

import oxygen.predef.core.*
import scala.annotation.tailrec
import scala.reflect.TypeTest

trait PathCodec[A] {

  /**
    * Potential paths this can parse/produce.
    *
    * Growable(
    *   Growable("by", "name", Param),
    *   Growable("id", Param),
    * )
    *
    * represents:
    * /by/name/%
    * /id/%
    */
  val specs: Growable[Growable[PathCodec.Spec]]

  def decode(paths: List[String]): Option[(A, List[String])]

  def encode(value: A): Growable[String]

  final def /[B](that: PathCodec[B])(using zip: Zip[A, B]): PathCodec[zip.Out] = PathCodec.AndThen(this, that, zip)

  final def <||[B >: A](that: PathCodec[B])(using ct: TypeTest[B, A]): PathCodec[B] = PathCodec.OrElse(this, that, ct)
  final def ||>[B <: A](that: PathCodec[B])(using ct: TypeTest[A, B]): PathCodec[A] = PathCodec.OrElse(that, this, ct)

  // TODO (KR) : transform

}
object PathCodec {

  def specsFor(codecs: PathCodec[?]*): List[List[PathCodec.Spec]] = {
    @tailrec
    def loop(
        queue: List[PathCodec[?]],
        acc: Growable[Growable[PathCodec.Spec]],
    ): Growable[Growable[PathCodec.Spec]] =
      queue match {
        case head :: tail =>
          loop(
            tail,
            for {
              a <- acc
              b <- head.specs
            } yield a ++ b,
          )
        case Nil =>
          acc
      }

    loop(codecs.toList, Growable.single(Growable.empty)).map(_.to[List]).to[List]
  }

  enum Spec {
    case Const(const: String)
    case Param
    case AllRemaining
    case AllRemainingNonEmpty
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given stringParam: PathCodec[String] = PathCodec.SingleStringParam
  given typedParam: [A: StringCodec as codec] => PathCodec[A] = PathCodec.SingleTypedParam(codec)
  given allRemaining: PathCodec[List[String]] = PathCodec.AllRemaining
  given allRemainingNonEmpty: PathCodec[NonEmptyList[String]] = PathCodec.AllRemainingNonEmpty

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Const(const: String) extends PathCodec[Unit] {

    override val specs: Growable[Growable[Spec]] = Growable.single(Growable.single(Spec.Const(const)))

    override def decode(paths: List[String]): Option[(Unit, List[String])] = paths match
      case `const` :: tail => ((), tail).some
      case _               => None

    override def encode(value: Unit): Growable[String] =
      Growable.single(const)

  }

  final case class Consts(consts: List[String]) extends PathCodec[Unit] {

    @tailrec
    private def loop(consts: List[String], paths: List[String]): Option[(Unit, List[String])] =
      consts match {
        case constsH :: constsT =>
          paths match {
            case `constsH` :: pathsT => loop(constsT, pathsT)
            case _                   => None
          }
        case Nil => ((), paths).some
      }

    override val specs: Growable[Growable[Spec]] = Growable.single(Growable.many(consts).map(Spec.Const(_)))

    override def encode(value: Unit): Growable[String] = Growable.many(consts)

    override def decode(paths: List[String]): Option[(Unit, List[String])] = loop(consts, paths)

  }

  case object SingleStringParam extends PathCodec[String] {

    override val specs: Growable[Growable[Spec]] = Growable.single(Growable.single(Spec.Param))

    override def encode(value: String): Growable[String] = Growable.single(value)

    override def decode(paths: List[String]): Option[(String, List[String])] = paths match
      case head :: tail => (head, tail).some
      case _            => None

  }

  final case class SingleTypedParam[A](codec: StringCodec[A]) extends PathCodec[A] {

    override val specs: Growable[Growable[Spec]] = Growable.single(Growable.single(Spec.Param))

    override def decode(paths: List[String]): Option[(A, List[String])] = paths match
      case head :: tail => codec.decoder.decodeSimple(head).toOption.map((_, tail))
      case _            => None

    override def encode(value: A): Growable[String] =
      Growable.single(codec.encoder.encode(value))

  }

  // TODO (KR) : Allow for a typed version of this

  case object AllRemaining extends PathCodec[List[String]] {

    override val specs: Growable[Growable[Spec]] = Growable.single(Growable.single(Spec.AllRemaining))

    override def decode(paths: List[String]): Option[(List[String], List[String])] =
      (paths, Nil).some

    override def encode(value: List[String]): Growable[String] =
      Growable.many(value)

  }

  case object AllRemainingNonEmpty extends PathCodec[NonEmptyList[String]] {

    override val specs: Growable[Growable[Spec]] = Growable.single(Growable.single(Spec.AllRemainingNonEmpty))

    override def decode(paths: List[String]): Option[(NonEmptyList[String], List[String])] =
      paths.toNonEmpty.map((_, Nil))

    override def encode(value: NonEmptyList[String]): Growable[String] =
      Growable.many(value.toList)

  }

  final case class AndThen[A, B, C](a: PathCodec[A], b: PathCodec[B], zip: Zip.Out[A, B, C]) extends PathCodec[C] {

    override val specs: Growable[Growable[Spec]] =
      for {
        aSpecs <- a.specs
        bSpecs <- b.specs
      } yield aSpecs ++ bSpecs

    override def decode(paths: List[String]): Option[(C, List[String])] =
      for {
        (aValue, aRes) <- a.decode(paths)
        (bValue, bRes) <- b.decode(aRes)
      } yield (zip.zip(aValue, bValue), bRes)

    override def encode(value: C): Growable[String] = {
      val (aValue, bValue) = zip.unzip(value)
      a.encode(aValue) ++ b.encode(bValue)
    }

  }

  final case class OrElse[A, B >: A](a: PathCodec[A], b: PathCodec[B], tt: TypeTest[B, A]) extends PathCodec[B] {

    override val specs: Growable[Growable[Spec]] = a.specs ++ b.specs

    override def decode(paths: List[String]): Option[(B, List[String])] =
      a.decode(paths).orElse(b.decode(paths))

    override def encode(value: B): Growable[String] = value match
      case tt(aValue) => a.encode(aValue)
      case _          => b.encode(value)

  }

}
