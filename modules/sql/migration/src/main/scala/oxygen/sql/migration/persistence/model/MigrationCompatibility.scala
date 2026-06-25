package oxygen.sql.migration.persistence.model

import oxygen.json.JsonCodec

/**
  * Classification of a generated migration, produced by the migration generator.
  *
  * Does double duty: it gates whether generation is allowed, and it drives the semver
  * version bump (backwards-compatible -> minor, incompatible -> major).
  */
enum MigrationCompatibility {
  case BackwardsCompatible
  case Incompatible
}
object MigrationCompatibility {

  def show(self: MigrationCompatibility): String = self match
    case BackwardsCompatible => "BackwardsCompatible"
    case Incompatible        => "Incompatible"

  def fromString(string: String): Either[String, MigrationCompatibility] = string match
    case "BackwardsCompatible" => Right(BackwardsCompatible)
    case "Incompatible"        => Right(Incompatible)
    case _                     => Left(s"invalid MigrationCompatibility: $string")

  given JsonCodec[MigrationCompatibility] = JsonCodec.string.transformOrFail(fromString, show)

}
