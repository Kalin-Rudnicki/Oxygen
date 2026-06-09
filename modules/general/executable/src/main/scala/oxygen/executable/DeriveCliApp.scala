package oxygen.executable

trait DeriveCliApp[R, A] {
  def app(value: A): CompiledCliApp[R]
}
object DeriveCliApp {

  trait Root[A] {
    def app: CompiledCliApp[Any]
  }
  object Root {
    inline given derived: [A] => DeriveCliApp.Root[A] = ${ oxygen.executable.generic.DeriveCliApp.derive[A] }
  }

}
