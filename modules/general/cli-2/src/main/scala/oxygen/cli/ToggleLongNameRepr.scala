package oxygen.cli

import oxygen.predef.core.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      ToggleLongNameRepr
//////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * Resolved long names for a [[toggle]] param.
  *
  * Exactly one naming strategy:
  *   - [[PrefixTrue]] / [[PrefixFalse]] / [[PrefixBoth]] — compose `prefix-base` flags from a shared [[base]]
  *   - [[Explicit]] — independent full long names for the true and false states
  *
  * Combining prefix annotations with trueName/falseName annotations is invalid.
  */
sealed trait ToggleLongNameRepr {

  def trueLongName: String
  def falseLongName: String

}
object ToggleLongNameRepr {

  final case class PrefixTrue(
      truePrefix: String,
      base: String,
  ) extends ToggleLongNameRepr {

    override val trueLongName: String = joinPrefix(truePrefix, base)
    override val falseLongName: String = base

  }

  final case class PrefixFalse(
      falsePrefix: String,
      base: String,
  ) extends ToggleLongNameRepr {

    override val trueLongName: String = base
    override val falseLongName: String = joinPrefix(falsePrefix, base)

  }

  final case class PrefixBoth(
      truePrefix: String,
      falsePrefix: String,
      base: String,
  ) extends ToggleLongNameRepr {

    override val trueLongName: String = joinPrefix(truePrefix, base)
    override val falseLongName: String = joinPrefix(falsePrefix, base)

  }

  final case class Explicit(
      trueName: String,
      falseName: String,
  ) extends ToggleLongNameRepr {

    override val trueLongName: String = trueName
    override val falseLongName: String = falseName

  }

  private def joinPrefix(prefix: String, base: String): String =
    s"$prefix-$base"

  def resolve(
      baseName: String,
      truePrefix: Option[String],
      falsePrefix: Option[String],
      trueName: Option[String],
      falseName: Option[String],
  ): Either[String, ToggleLongNameRepr] = {
    val hasPrefixAnnot: Boolean = truePrefix.isDefined || falsePrefix.isDefined
    val hasNameAnnot: Boolean = trueName.isDefined || falseName.isDefined

    if hasPrefixAnnot && hasNameAnnot then
      "Cannot combine longName prefix annotations (truePrefix/falsePrefix) with explicit longName annotations (trueName/falseName)".asLeft

    else if hasNameAnnot then
      (trueName, falseName) match
        case (Some(t), Some(f)) => Explicit(t, f).asRight
        case _                  => "Toggle requires both trueName and falseName when using explicit long names".asLeft

    else if hasPrefixAnnot then
      (truePrefix, falsePrefix) match
        case (Some(tp), None)     => PrefixTrue(tp, baseName).asRight
        case (None, Some(fp))     => PrefixFalse(fp, baseName).asRight
        case (Some(tp), Some(fp)) => PrefixBoth(tp, fp, baseName).asRight
        case (None, None)         => "Internal Defect : prefix annotations present but both are None".asLeft

    else
      "Toggle requires either prefix annotations (truePrefix/falsePrefix) or explicit long names (trueName/falseName)".asLeft

  }

}