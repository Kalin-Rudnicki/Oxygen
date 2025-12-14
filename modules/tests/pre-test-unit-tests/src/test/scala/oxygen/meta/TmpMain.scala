package oxygen.meta

object TmpMain extends scala.App {

  val fib: Int => Int =
    Macros.cachedFunction { (i: Int) =>
      if i == 0 || i == 1 then 1
      else {
        println(s"rec($i)(${i - 1}, ${i - 2})")
        fib(i - 1) + fib(i - 2)
      }
    }

  println(s"result.1 = ${fib(5)}")
  println()
  println(s"result.2 = ${fib(7)}")

}
