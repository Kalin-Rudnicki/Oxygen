package oxygen.core

import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.option.*
import oxygen.core.typeclass.{Show, StringCodec}
import scala.annotation.tailrec
import scala.quoted.*

final case class Version(
    hasVPrefix: Boolean,
    coreVersions: NonEmptyList[Int],
    suffix: Option[Version.Suffix],
) {

  lazy val (major, minor, patch): (Int, Int, Int) = coreVersions.toList match
    case major :: minor :: patch :: _ => (major, minor, patch)
    case major :: minor :: _          => (major, minor, 0)
    case major :: _                   => (major, 0, 0)
    case _                            => throw new RuntimeException("not possible... NEL...")

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Version => Version.ordering.equiv(this, that)
    case _             => false

  override def toString: String =
    coreVersions.mkString(if (hasVPrefix) "v" else "", ".", suffix.fold("")(_.toString))

}
object Version {

  private val reg = "^(v)?([0-9]+(?:\\.[0-9]+)*)(-.+)?$".r
  private val rcReg = "^-RC([0-9]+)$".r
  private val intOrd: Ordering[Int] = Ordering[Int]
  private val strOrd: Ordering[String] = Ordering[String]

  enum Suffix {
    case Snapshot(suffix: String)
    case Other(suffix: String)
    case ReleaseCandidate(rc: Int)

    override final def toString: String = this match {
      case Suffix.Snapshot(suffix)     => suffix
      case Suffix.Other(suffix)        => suffix
      case Suffix.ReleaseCandidate(rc) => s"-RC$rc"
    }

  }
  object Suffix {

    given ordering: Ordering[Suffix] = {
      case (a: Suffix.Snapshot, b: Suffix.Snapshot)                 => strOrd.compare(a.suffix, b.suffix)
      case (a: Suffix.Other, b: Suffix.Other)                       => strOrd.compare(a.suffix, b.suffix)
      case (a: Suffix.ReleaseCandidate, b: Suffix.ReleaseCandidate) => intOrd.compare(a.rc, b.rc)
      case (a, b)                                                   => intOrd.compare(a.ordinal, b.ordinal)
    }

    def parse(suffix: String): Suffix = suffix match
      case rcReg(rc)                                => Suffix.ReleaseCandidate(rc.toInt)
      case _ if suffix.toUpperCase.contains("SNAP") => Suffix.Snapshot(suffix)
      case _                                        => Suffix.Other(suffix)

  }

  given ordering: Ordering[Version] = { (versionA, versionB) =>
    @tailrec
    def loop(a: List[Int], b: List[Int]): Int =
      (a, b) match {
        case (aHead :: aTail, bHead :: bTail) =>
          if (aHead < bHead) -1
          else if (aHead > bHead) 1
          else loop(aTail, bTail)
        case (rest @ _ :: _, Nil) if rest.exists(_ != 0) => 1
        case (Nil, rest @ _ :: _) if rest.exists(_ != 0) => -1
        case (_, _)                                      =>
          (versionA.suffix, versionB.suffix) match {
            case (None, None)                   => 0
            case (Some(aSuffix), Some(bSuffix)) => Suffix.ordering.compare(aSuffix, bSuffix)
            case (Some(_), None)                => -1
            case (None, Some(_))                => 1
          }
      }

    loop(versionA.coreVersions.toList, versionB.coreVersions.toList)
  }

  given show: Show[Version] = Show.usingToString
  given stringCode: StringCodec[Version] = StringCodec.string.transformOption(parse(_), _.toString)

  private def applyImpl(versionExpr: Expr[String])(using quotes: Quotes): Expr[Version] = {
    import quotes.reflect.*
    val versionStr: String = versionExpr.valueOrAbort
    val version: Version = parse(versionStr).getOrElse { report.errorAndAbort(s"invalid version: $versionStr") }

    def suffixExpr(suffix: Suffix): Expr[Suffix] =
      suffix match {
        case Suffix.Snapshot(suffix)     => '{ Suffix.Snapshot(${ Expr(suffix) }) }
        case Suffix.Other(suffix)        => '{ Suffix.Other(${ Expr(suffix) }) }
        case Suffix.ReleaseCandidate(rc) => '{ Suffix.ReleaseCandidate(${ Expr(rc) }) }
      }

    '{
      Version(
        ${ Expr(version.hasVPrefix) },
        NonEmptyList(${ Expr(version.coreVersions.head) }, ${ Expr(version.coreVersions.tail) }),
        ${
          version.suffix match {
            case Some(value) => '{ Some(${ suffixExpr(value) }) }
            case None        => '{ None }
          }
        },
      )
    }
  }

  inline def apply(version: String): Version = ${ applyImpl('version) }

  def make(major: Int, extras: Int*): Version = Version(false, NonEmptyList(major, extras.toList), None)

  def parse(version: String): Option[Version] = version match
    case reg(vPrefix, coreVersions, suffix) => Version(vPrefix != null, NonEmptyList.unsafeFromList(coreVersions.split('.').toList.map(_.toInt)), Option(suffix).map(Suffix.parse)).some
    case _                                  => None

  def unsafeParse(version: String): Version =
    parse(version).getOrElse { throw new RuntimeException(s"Invalid version: $version") }

}
