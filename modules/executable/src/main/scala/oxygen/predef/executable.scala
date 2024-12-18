package oxygen.predef

object executable {
  // TODO (KR) : move these to a CLI predef?
  export oxygen.cli.{Arg, LongName, Name, ShortName}
  export oxygen.cli.{charToDefaultableName, charToShortName, stringToLongName}
  export oxygen.cli.{Params, Parser, Values}
  // TODO (KR) : add actual executable things
}
