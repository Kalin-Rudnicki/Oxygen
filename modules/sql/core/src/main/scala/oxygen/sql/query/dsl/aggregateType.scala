package oxygen.sql.query.dsl

/**
  * Type-level widening for the `SUM(_)` aggregate, mirroring Postgres' result types.
  *
  * Postgres widens the result of `SUM`:
  *   - `smallint` / `int`      -> `bigint`  (Scala `Long`)
  *   - `bigint`                -> `numeric` (Scala `BigInt`, an integral `numeric`)
  *   - `numeric`               -> `numeric` (Scala `BigDecimal`)
  *   - `real`                  -> `real`    (Scala `Float`)
  *   - `double precision`      -> `double precision` (Scala `Double`)
  *
  * The resulting DSL expression decodes to `Option[Out]` (SQL `NULL` over an empty set -> `None`).
  */
sealed trait SumType[A] {
  type Out
}
object SumType {

  type Aux[A, B] = SumType[A] { type Out = B }

  private def make[A, B]: SumType.Aux[A, B] = new SumType[A] { override type Out = B }

  given short: SumType.Aux[Short, Long] = make
  given int: SumType.Aux[Int, Long] = make
  given long: SumType.Aux[Long, BigInt] = make
  given bigInt: SumType.Aux[BigInt, BigInt] = make
  given float: SumType.Aux[Float, Float] = make
  given double: SumType.Aux[Double, Double] = make
  given bigDecimal: SumType.Aux[BigDecimal, BigDecimal] = make

}

/**
  * Type-level widening for the `AVG(_)` aggregate, mirroring Postgres' result types.
  *
  * Postgres returns:
  *   - `numeric` for `smallint` / `int` / `bigint` / `numeric` inputs (Scala `BigDecimal`)
  *   - `double precision` for `real` / `double precision` inputs (Scala `Double`)
  *
  * (a Scala `BigInt` column is itself a `numeric`, so `AVG` over it is `BigDecimal`.)
  *
  * The resulting DSL expression decodes to `Option[Out]` (SQL `NULL` over an empty set -> `None`).
  */
sealed trait AvgType[A] {
  type Out
}
object AvgType {

  type Aux[A, B] = AvgType[A] { type Out = B }

  private def make[A, B]: AvgType.Aux[A, B] = new AvgType[A] { override type Out = B }

  given short: AvgType.Aux[Short, BigDecimal] = make
  given int: AvgType.Aux[Int, BigDecimal] = make
  given long: AvgType.Aux[Long, BigDecimal] = make
  given bigInt: AvgType.Aux[BigInt, BigDecimal] = make
  given float: AvgType.Aux[Float, Double] = make
  given double: AvgType.Aux[Double, Double] = make
  given bigDecimal: AvgType.Aux[BigDecimal, BigDecimal] = make

}
