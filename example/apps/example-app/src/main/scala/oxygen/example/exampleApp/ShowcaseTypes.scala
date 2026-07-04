package oxygen.example.exampleApp

import oxygen.cli.*
import oxygen.predef.core.*
import oxygen.schema.{JsonSchema, PlainTextSchema}

/** Shared context from root `def env`. */
final case class RootCtx(
    host: String,
    metrics: Boolean,
    shard: Int,
    settings: Option[ShowcaseSettings],
    tags: List[String],
    quiet: Option[String],
)

final case class WorkspaceCtx(
    workspaceId: String,
    connected: Boolean,
)

final case class GizmoCtx(
    gizmoId: String,
    calibrated: Boolean,
)

final case class ShowcaseSettings(
    theme: String,
    retries: Int,
) derives JsonSchema

final case class TaskConfig(
    action: String,
    priority: Int,
) derives JsonSchema

enum Priority derives StrictEnum {
  case Low, Normal, High, Critical
}

enum OutputFormat derives StrictEnum {
  case Text, Json, Yaml
}

/** Parsed by a custom [[ArgsParser]] (three positional ints). */
final case class RgbColor(r: Int, g: Int, b: Int)

object RgbColor {

  given ArgsParser[RgbColor] =
    (
      PositionalArgsParser.singlePlain[Int]("r") ^>>
        PositionalArgsParser.singlePlain[Int]("g") ^>>
        PositionalArgsParser.singlePlain[Int]("b")
    ).map { case (r, g, b) => RgbColor(r, g, b) }

}
