package oxygen.core

object TmpUse {

  final case class typeName(name: String) extends scala.annotation.Annotation
  final case class fieldName(name: String) extends scala.annotation.Annotation

  @typeName("my-class")
  final case class MyClass(
      @fieldName("my-int") int: Int,
  )

  TmpMacro.showType[MyClass]

}
