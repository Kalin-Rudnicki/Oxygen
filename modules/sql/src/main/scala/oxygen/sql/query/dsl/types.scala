package oxygen.sql.query.dsl

final class SelectNoInput private[dsl] () {
  def debug: SelectNoInput = macroOnly
}

final class SelectInput[I] private[dsl] () {
  def debug: SelectInput[I] = macroOnly
}

final class From[I] private[dsl] () {

  def map[O](f: I => O): Returning[O] = macroOnly
  def flatMap[O](f: I => Returning[O]): Returning[O] = macroOnly

}

final class Join[I] private[dsl] () {

  def withFilter(f: I => Boolean): JoinApplied[I] = macroOnly

}

final class Where private[dsl] () {

  def withFilter(f: Unit => Boolean): WhereApplied = macroOnly

}

final class Returning[O] private[dsl] ()

final class JoinApplied[I] private[dsl] () {

  def map[O](f: I => O): Returning[O] = macroOnly
  def flatMap[O](f: I => Returning[O]): Returning[O] = macroOnly

}

final class WhereApplied private[dsl] () {

  def map[O](f: Unit => O): Returning[O] = macroOnly
  def flatMap[O](f: Unit => Returning[O]): Returning[O] = macroOnly

}
