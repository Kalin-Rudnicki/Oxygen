package oxygen.cli

final case class CompletionRequest(numWords: Int, argIdx: Int, args: List[String], joinStr: String)
object CompletionRequest {
  val numWordsEnv = "OXYGEN_CLI_COMPLETE__NUM_WORDS"
  val argIdxEnv = "OXYGEN_CLI_COMPLETE__ARG_IDX"
  val accArgsEnv = "OXYGEN_CLI_COMPLETE__ACC_ARGS"
  val joinStrEnv = "OXYGEN_CLI_COMPLETE__JOIN_STR"
}
