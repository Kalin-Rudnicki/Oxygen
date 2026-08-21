package oxygen.sql.migration.compat

import oxygen.predef.core.*
import oxygen.sql.migration.persistence.model.MigrationCompatibility
import oxygen.sql.schema.Column

/**
  * Compatibility verdict for a single column-type transition `from -> to`.
  *
  * The verdict is intentionally conservative / fail-closed: only the explicitly-enumerated
  * lossless widenings are [[MigrationCompatibility.BackwardsCompatible]]; their reverses are
  * narrowings, and every other cross-type change is [[TypeComparison.Changed]] -- all
  * [[MigrationCompatibility.Incompatible]]. `Array` delegates to its element type.
  */
enum TypeComparison {

  case Same(tpe: Column.Type)
  case Widening(fromType: Column.Type, toType: Column.Type)
  case Narrowing(fromType: Column.Type, toType: Column.Type)
  case Changed(fromType: Column.Type, toType: Column.Type)
  case ArrayElem(fromType: Column.Type, toType: Column.Type, underlying: TypeComparison)

  lazy val compatibility: MigrationCompatibility = this match
    case _: TypeComparison.Same         => MigrationCompatibility.BackwardsCompatible
    case _: TypeComparison.Widening     => MigrationCompatibility.BackwardsCompatible
    case _: TypeComparison.Narrowing    => MigrationCompatibility.Incompatible
    case _: TypeComparison.Changed      => MigrationCompatibility.Incompatible
    case self: TypeComparison.ArrayElem => self.underlying.compatibility

  final def isCompatible: Boolean = compatibility == MigrationCompatibility.BackwardsCompatible

  final def isDifferent: Boolean = this match
    case _: TypeComparison.Same         => false
    case self: TypeComparison.ArrayElem => self.underlying.isDifferent
    case _                              => true

  def toIndentedString: IndentedString = this match
    case TypeComparison.Same(tpe)              => s"Same(${tpe.show})"
    case TypeComparison.Widening(f, t)         => s"Widening(${f.show} -> ${t.show})"
    case TypeComparison.Narrowing(f, t)        => s"Narrowing(${f.show} -> ${t.show})"
    case TypeComparison.Changed(f, t)          => s"Changed(${f.show} -> ${t.show})"
    case TypeComparison.ArrayElem(f, t, under) => IndentedString.keyValueSection(s"ArrayElem(${f.show} -> ${t.show}):")("elem: " -> under.toIndentedString)

}
object TypeComparison {

  /**
    * Directed lossless-widening pairs (Postgres-safe). A transition present here is a widening; its
    * reverse is a narrowing; anything else is [[TypeComparison.Changed]] (fail-closed incompatible).
    */
  private val wideningPairs: Set[(Column.Type, Column.Type)] =
    Set(
      (Column.Type.SmallInt, Column.Type.Int),
      (Column.Type.SmallInt, Column.Type.BigInt),
      (Column.Type.Int, Column.Type.BigInt),
      (Column.Type.Real, Column.Type.DoublePrecision),
    )

  def compare(from: Column.Type, to: Column.Type): TypeComparison =
    (from, to) match
      case (a, b) if a == b                             => TypeComparison.Same(a)
      case (Column.Type.Array(a), Column.Type.Array(b)) => TypeComparison.ArrayElem(from, to, compare(a, b))
      case (f, t) if wideningPairs.contains((f, t))     => TypeComparison.Widening(f, t)
      case (f, t) if wideningPairs.contains((t, f))     => TypeComparison.Narrowing(f, t)
      case (f, t)                                       => TypeComparison.Changed(f, t)

}
