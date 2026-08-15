# Scala coding standards

Stub — expand over time (see OXY-168).

## File naming

- A file with a **single** top-level class/object/trait uses the **PascalCase** name of that type (e.g. `RowRepr.scala`).
- A file with **multiple** top-level classes uses a **lower-case** name (e.g. `aggregateType.scala` holding `SumType` + `AvgType`).
  - **Exception:** a typeclass paired with its low-priority companion — `MyTypeclass` + `MyTypeclassLowPriority` — stays under the PascalCase `MyTypeclass.scala`.
