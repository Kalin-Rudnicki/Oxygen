package oxygen.executable.generic

import oxygen.cli.ToggleLongNameRepr as CliToggleLongNameRepr

private[generic] object ToggleLongNameRepr {

  def extract(raw: RawParamRepr): CliToggleLongNameRepr =
    CliToggleLongNameRepr
      .resolve(
        baseName = raw.annot_longName.fold(raw.valDef.name)(_.name),
        truePrefix = raw.annot_longName_truePrefix.map(_.prefix),
        falsePrefix = raw.annot_longName_falsePrefix.map(_.prefix),
        trueName = raw.annot_longName_trueName.map(_.name),
        falseName = raw.annot_longName_falseName.map(_.name),
      )
      .fold(raw.failAtVal(_), identity)

}