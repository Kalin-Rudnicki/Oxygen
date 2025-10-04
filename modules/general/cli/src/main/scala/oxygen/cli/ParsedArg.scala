package oxygen.cli

sealed trait ParsedArg {
  val names: List[LongReference]
  val args: List[Arg]

  final def simpleShow: String = names.headOption.fold("<unknown>")(_.showParam.toString)

}
final case class ParsedValueArg(names: List[LongName], args: List[Arg.ValueLike]) extends ParsedArg
final case class ParsedParamArg(names: List[LongReference], args: List[Arg.ParamLike]) extends ParsedArg
