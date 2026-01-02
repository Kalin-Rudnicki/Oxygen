package oxygen.core

import oxygen.core.collection.*
import oxygen.core.syntax.option.*
import oxygen.core.typeclass.{Show, StringCodec}
import oxygen.meta.{*, given}
import oxygen.quoted.*
import scala.annotation.tailrec
import scala.quoted.*

final case class Version private (
    raw: Specified[String],
    repr: Version.Repr,
) {

  lazy val showRepr: String = repr.show(false)
  lazy val showRawOrRepr: String = raw.getOrElse(showRepr)
  def show: String = showRawOrRepr
  override def toString: String = showRawOrRepr

  def major: Int = repr.major
  def minor: Int = repr.minor
  def patch: Int = repr.patch

  override def hashCode: Int = repr.hashCode
  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: Version => this.repr == that.repr
    case _             => false

}
object Version {

  def parse(version: String): Option[Version] =
    Version.parsing.parse(version).map(Version(version, _))

  def unsafeParse(version: String): Version =
    parse(version).getOrElse { throw new RuntimeException(s"Invalid version: $version") }

  def fromRepr(repr: Version.Repr): Version =
    Version(___, repr)

  def make(coreVersions: NonEmptyList[Int], suffix: Suffix, offset: Option[GitTagOffset], isSnapshot: Boolean): Version =
    Version.fromRepr(Repr(coreVersions, suffix, offset, isSnapshot))

  def make(coreVersion0: Int, coreVersionN: Int*)(suffix: Specified[Suffix] = ___, offset: Specified[GitTagOffset] = ___, isSnapshot: Specified[Boolean] = ___): Version = {
    val suf = suffix.getOrElse(ReleaseSuffix.Empty)
    val off = offset.toOption
    val snap = isSnapshot.getOrElse { off.nonEmpty }
    Version.make(NonEmptyList(coreVersion0, coreVersionN.toList), suf, off, snap)
  }

  inline def apply(inline version: String): Version = ${ applyImpl('version, 'false) }
  inline def apply(inline version: String, inline debug: Boolean): Version = ${ applyImpl('version, 'debug) }

  given ordering: Ordering[Version] = Ordering.by(_.repr)
  given stringCodec: StringCodec[Version] = StringCodec.string.transformOption(parse, _.showRawOrRepr)
  given show: Show[Version] = v => v.raw.getOrElse { v.repr.show(true) }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class GitTagOffset(
      offset: Int,
      sha: String,
  ) {
    def show: String = s"+$offset-g$sha"
    override def toString: String = this.show
  }
  object GitTagOffset {

    val empty: GitTagOffset = GitTagOffset(0, "0")

    private val reg = "^([1-9][0-9]*)-g?([0-9a-f]+)$".r

    def fromString(value: String): Option[GitTagOffset] = value match
      case reg(offset, sha) => GitTagOffset(offset.toInt, sha).some
      case _                => None

    def unapply(value: String): Option[GitTagOffset] = fromString(value)

    given ordering: Ordering[GitTagOffset] =
      Ordering.by(_.offset)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Repr
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Repr {

    val coreVersions: NonEmptyList[Int]
    val suffix: Suffix
    val optOffset: Option[GitTagOffset]
    val isSnapshot: Boolean
    val addSnapshotSuffix: Boolean

    final def isExact: Boolean = optOffset.isEmpty

    final def show: String = show(false)
    final def show(vPrefix: Boolean): String =
      s"${if vPrefix then "v" else ""}${coreVersions.mkString(".")}${suffix.show}${optOffset.fold("")(_.show)}${if addSnapshotSuffix then "-SNAPSHOT" else ""}"

    final def gitOffsetImpliesSnapshot: Repr = this match
      case self: Repr.WithGitTagOffset       => Repr.Snapshot(self)
      case self: (Repr.Core | Repr.Snapshot) => self

    lazy val (major, minor, patch): (Int, Int, Int) = coreVersions.toList match
      case major :: minor :: patch :: _ => (major, minor, patch)
      case major :: minor :: _          => (major, minor, 0)
      case major :: _                   => (major, 0, 0)
      case _                            => throw new RuntimeException("not possible... NEL...")

    override final lazy val hashCode: Int =
      (coreVersions.toList.reverse.dropWhile(_ == 0), suffix.toComparableString, optOffset.fold(0)(_.offset), isSnapshot).hashCode
    override final def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
      case that: Repr => Repr.ordering.equiv(this, that)
      case _          => false

    override def toString: String = s"Repr { coreVersion: ${coreVersions.mkString(".")} | suffix: $suffix | offset: ${optOffset.fold("<none>")(_.show)} | isSnapshot: $isSnapshot }"

  }
  object Repr {

    def apply(coreVersions: NonEmptyList[Int], suffix: Suffix, offset: Option[GitTagOffset], isSnapshot: Boolean): Repr = {
      val core: Repr.Core = Repr.Core(coreVersions, suffix)
      (suffix, offset, isSnapshot) match
        case (_, None, false)                  => core
        case (_, Some(offset), false)          => Repr.WithGitTagOffset(core, offset)
        case (ReleaseSuffix.Empty, None, true) => Repr.Core(coreVersions, PreReleaseSuffix.Snapshot)
        case (_, None, true)                   => Repr.Snapshot(core)
        case (_, Some(offset), true)           => Repr.Snapshot(Repr.WithGitTagOffset(core, offset))

    }

    sealed trait NonSnapshot extends Repr

    final case class Core(coreVersions: NonEmptyList[Int], suffix: Suffix) extends Repr.NonSnapshot {
      override val optOffset: Option[GitTagOffset] = None
      override val isSnapshot: Boolean = suffix == PreReleaseSuffix.Snapshot
      override val addSnapshotSuffix: Boolean = false
    }

    final case class WithGitTagOffset(underlying: Repr.Core, offset: GitTagOffset) extends NonSnapshot {
      override val coreVersions: NonEmptyList[Int] = underlying.coreVersions
      override val suffix: Suffix = underlying.suffix
      override val optOffset: Option[GitTagOffset] = offset.some
      override val isSnapshot: Boolean = underlying.isSnapshot
      override val addSnapshotSuffix: Boolean = false
    }

    final case class Snapshot(underlying: Repr.NonSnapshot) extends Repr {
      override val coreVersions: NonEmptyList[Int] = underlying.coreVersions
      override val suffix: Suffix = underlying.suffix
      override val optOffset: Option[GitTagOffset] = underlying.optOffset
      override val isSnapshot: Boolean = true
      override val addSnapshotSuffix: Boolean = !(underlying.isSnapshot && underlying.optOffset.isEmpty)
    }

    given ordering: Ordering[Repr] = {
      @tailrec
      def compareCoreVersions(x: List[Int], y: List[Int]): ComparisonResult =
        (x, y) match {
          case (Nil, Nil) =>
            ComparisonResult.EqualTo
          case _ =>
            val (xHead, xTail) = x match
              case head :: tail => (head, tail)
              case Nil          => (0, Nil)
            val (yHead, yTail) = y match
              case head :: tail => (head, tail)
              case Nil          => (0, Nil)
            if xHead < yHead then ComparisonResult.LessThan
            else if xHead > yHead then ComparisonResult.GreaterThan
            else compareCoreVersions(xTail, yTail)
        }

      val coreVersionOrdering: Ordering[Repr] =
        ComparisonResult.makeOrdering[Repr] { (x, y) => compareCoreVersions(x.coreVersions.toList, y.coreVersions.toList) }

      val suffixOrdering: Ordering[Repr] =
        Ordering.by(_.suffix)

      val offsetOrdering: Ordering[Repr] =
        Ordering.by(_.optOffset.getOrElse(GitTagOffset.empty))

      val snapshotOrdering: Ordering[Repr] =
        Ordering.by { r => if r.isSnapshot then 0 else 1 }

      coreVersionOrdering.orElse(suffixOrdering.orElse(offsetOrdering.orElse(snapshotOrdering)))
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Suffix
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Suffix {

    val isExplicit: Boolean = true

    def toComparableString: String
    def show: String

  }
  object Suffix {

    given ordering: Ordering[Suffix] =
      ComparisonResult.makeOrdering[Suffix] {
        case (x: ReleaseSuffix, y: ReleaseSuffix)       => ComparisonResult(ReleaseSuffix.ordering.compare(x, y))
        case (x: PreReleaseSuffix, y: PreReleaseSuffix) => ComparisonResult(PreReleaseSuffix.ordering.compare(x, y))
        case (_: ReleaseSuffix, _: PreReleaseSuffix)    => ComparisonResult.GreaterThan
        case (_: PreReleaseSuffix, _: ReleaseSuffix)    => ComparisonResult.LessThan
      }

  }

  sealed trait ReleaseSuffix extends Suffix {
    val versionSuffix: Specified[Int]
  }
  object ReleaseSuffix {

    case object Empty extends ReleaseSuffix {
      override val isExplicit: Boolean = false
      override val versionSuffix: Specified[ComparisonResult] = ___
      override def show: String = ""
      override def toComparableString: String = ""
    }
    case object Release extends ReleaseSuffix {
      override val versionSuffix: Specified[ComparisonResult] = ___
      override def show: String = "-release"
      override def toComparableString: String = ""
    }
    final case class Final(versionSuffix: Specified[Int]) extends ReleaseSuffix {
      override def show: String = ".Final" + versionSuffix.fold("")(_.toString)
      override def toComparableString: String = ".Final" + versionSuffix.fold("0")(_.toString)
    }

    given ordering: Ordering[ReleaseSuffix] =
      Ordering.by(_.versionSuffix.getOrElse(0))

  }

  sealed trait PreReleaseSuffix extends Suffix {
    def raw: String
    override final def show: String = "-" + raw
  }
  object PreReleaseSuffix {

    sealed abstract class WellDefined(final val ordinal: Int, final val prefix: String) extends PreReleaseSuffix {
      val versionSuffix: Specified[Int]
      override final lazy val raw: String = prefix + versionSuffix.fold("")(_.toString)
      override final def toComparableString: String = prefix + versionSuffix.fold("0")(_.toString)
    }
    object WellDefined {

      given ordering: Ordering[WellDefined] =
        Ordering.by[WellDefined, Int](_.ordinal).orElseBy(_.versionSuffix.getOrElse(0))

    }

    final case class Alpha(versionSuffix: Specified[Int]) extends PreReleaseSuffix.WellDefined(1, "ALPHA")
    final case class Beta(versionSuffix: Specified[Int]) extends PreReleaseSuffix.WellDefined(2, "BETA")
    final case class Milestone(versionSuffix: Specified[Int]) extends PreReleaseSuffix.WellDefined(3, "M")
    final case class ReleaseCandidate(versionSuffix: Specified[Int]) extends PreReleaseSuffix.WellDefined(4, "RC")
    case object Snapshot extends PreReleaseSuffix.WellDefined(5, "SNAPSHOT") {
      override val versionSuffix: Specified[ComparisonResult] = ___
    }

    final case class Custom(raw: String) extends PreReleaseSuffix {
      override def toComparableString: String = raw
    }

    given ordering: Ordering[PreReleaseSuffix] =
      ComparisonResult.refinedOrElseBy[PreReleaseSuffix, WellDefined, String](_.raw)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      parsing
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private given gitTagOffsetToExpr: ToExprT[Version.GitTagOffset] = ToExprT.derived
  private given suffixToExpr: ToExprT[Version.Suffix] = ToExprT.derived

  private def applyImpl(valueExpr: Expr[String], debugExpr: Expr[Boolean])(using Quotes): Expr[Version] = {
    val value: String = valueExpr.evalRequired
    val debug: Boolean = debugExpr.evalRequired
    val version: Version = Version.parse(value).getOrElse { report.errorAndAbort(s"Invalid version: $value") }

    val reprExpr: Expr[Version.Repr] =
      '{ Version.Repr(${ Expr(version.repr.coreVersions) }, ${ Expr(version.repr.suffix) }, ${ Expr(version.repr.optOffset) }, ${ Expr(version.repr.isSnapshot) }) }
    val versionExpr: Expr[Version] =
      '{ Version(${ Expr(version.raw) }, $reprExpr) }

    if debug then
      report.info(
        s"""  version: ${version.showRepr}
           |     repr: ${version.repr}
           |    input: $value
           |""".stripMargin,
      )

    versionExpr
  }

  private object parsing {

    // =====| Regexes |=====

    private val num = "(?:0|[1-9][0-9]*)".r

    // TODO (KR) : consider attempting to fallback to standard semver parsing if oxygen version parsing logic fails
    // private val semverReg = "^([^-]+)(?:-([^+]+))?(?:\\+(.+))?$".r

    private val popCoreVersions = s"^v?($num(?:\\.$num)*)(.*)$$".r

    private val popReversedSnapshot = "^tohspans-*(.*)$".r
    private val popReversedGitOffset = "^([0-9a-f]+)g?-+([0-9]+)(?:-+|\\+|)(.*)$".r

    private val releaseSuffix = s"^(?:\\.|-+|)release$$".r
    private val finalSuffix = s"^(?:\\.|-+|)final(?:-*($num))?$$".r

    private val releaseCandidateSuffix = s"^(?:\\.|-+|)rc(?:-*($num))?$$".r
    private val milestoneSuffix = s"^(?:\\.|-+|)m(?:-*($num))?$$".r
    private val betaSuffix = s"^(?:\\.|-+|)beta(?:-*($num))?$$".r
    private val alphaSuffix = s"^(?:\\.|-+|)alpha(?:-*($num))?$$".r
    private val customSuffix = "^(?:\\.|-+|)([a-z0-9\\-]+)$".r

    private def optInt(in: String): Specified[Int] = Specified(in).map(_.toInt)

    def parse(input: String): Option[Repr] =
      for {
        (coreStr, pass1) <- input.toLowerCase match
          case popCoreVersions(core, rest) => (core, rest.reverse).some
          case _                           => None
        (isSnapshot, pass2) = pass1 match
          case popReversedSnapshot(rest) => (true, rest)
          case _                         => (false, pass1)
        (optOffset, pass3) = pass2 match
          case popReversedGitOffset(revSha, revOff, rest) => (GitTagOffset(revOff.reverse.toInt, revSha.reverse).some, rest.reverse)
          case _                                          => (None, pass2.reverse)
        suffix <-
          pass3 match
            case ""                                    => ReleaseSuffix.Empty.some
            case releaseSuffix()                       => ReleaseSuffix.Release.some
            case finalSuffix(versionSuffix)            => ReleaseSuffix.Final(optInt(versionSuffix)).some
            case releaseCandidateSuffix(versionSuffix) => PreReleaseSuffix.ReleaseCandidate(optInt(versionSuffix)).some
            case milestoneSuffix(versionSuffix)        => PreReleaseSuffix.Milestone(optInt(versionSuffix)).some
            case betaSuffix(versionSuffix)             => PreReleaseSuffix.Beta(optInt(versionSuffix)).some
            case alphaSuffix(versionSuffix)            => PreReleaseSuffix.Alpha(optInt(versionSuffix)).some
            case customSuffix(raw)                     => PreReleaseSuffix.Custom(raw).some
            case _                                     => None
      } yield Repr(NonEmptyList.unsafeFromList(coreStr.split('.').map(_.toInt).toList), suffix, optOffset, isSnapshot)

  }

}
