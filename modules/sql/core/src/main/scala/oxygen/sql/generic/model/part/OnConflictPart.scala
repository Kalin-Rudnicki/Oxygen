package oxygen.sql.generic.model.part

/**
  * The `ON CONFLICT` clause of an insert, targeting the table's primary-key columns.
  *   - [[DoNothing]] -> `ON CONFLICT (pk...) DO NOTHING`
  *   - [[DoUpdate]]  -> `ON CONFLICT (pk...) DO UPDATE SET <non-pk> = EXCLUDED.<non-pk>`
  *     (falls back to `DO NOTHING` when there are no non-primary-key columns).
  */
enum OnConflictPart {
  case DoNothing
  case DoUpdate
}
