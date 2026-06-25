package oxygen.sql.migration

import oxygen.core.Version
import oxygen.json.JsonCodec
import oxygen.sql.schema.RowRepr

/**
  * Shared codecs for representing [[oxygen.core.Version]] in the migration subsystem.
  *
  * Migrations are identified by semver [[Version]] (total `Ordering`, gaps allowed). Both the
  * filesystem migration files and the DB execution-tracking rows need to (de)serialize it:
  *   - [[versionJsonCodec]] -- for the on-disk filesystem files
  *   - [[versionRowRepr]]   -- so a `Version` can be a SQL column (stored as TEXT)
  *
  * `Version` lives in `oxygen-core`, which depends on neither `oxygen-json` nor `oxygen-sql`, so
  * these instances cannot live on its companion -- they are defined here and imported where needed.
  */
object MigrationCodecs {

  private def parse(string: String): Either[String, Version] =
    Version.parse(string).toRight(s"invalid version: $string")

  given versionJsonCodec: JsonCodec[Version] =
    JsonCodec.string.transformOrFail(parse, _.show)

  given versionRowRepr: RowRepr[Version] =
    RowRepr.string.transformOrFail(parse, _.show)

}
