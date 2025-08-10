package oxygen.predef

object executable {
  export oxygen.cli.{Arg, LongName, Name, ShortName}
  export oxygen.cli.{charToDefaultableName, charToShortName, stringToLongName}
  export oxygen.cli.{Params, Parser, Values}
  export oxygen.executable.{Executable, ExecutableApp}
}
