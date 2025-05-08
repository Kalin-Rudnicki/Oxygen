package oxygen.test

import oxygen.core.collection.Growable
import oxygen.zio.*
import oxygen.zio.logging.LogConfig
import oxygen.zio.syntax.log.*
import zio.*
import zio.oxygen.logOps
import zio.test.*

object OAspect {

  extension [_R](self: FiberRefModificationR[_R])
    def toTestAspect: TestAspect.PerTest.AtLeastR[_R] =
      new TestAspect.PerTest.AtLeastR[_R] {
        override def perTest[R <: _R, E](test: ZIO[R, TestFailure[E], TestSuccess])(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
          test @@ self
      }

  def usingConfig(config: LogConfig): TestAspect.PerTest.Poly =
    LogConfig.usingConfig(config).toTestAspect

  def withMinLogLevel(level: LogLevel): TestAspect.PerTest.Poly =
    logOps.withMinLogLevel(level).toTestAspect

  val withTestAsLogSpan: TestAspect[Nothing, Any, Nothing, Any] =
    new TestAspect[Nothing, Any, Nothing, Any] {

      def loop[R, E](acc: Growable[String], spec: Spec[R, E])(using trace: Trace): Spec[R, E] = {
        val res: Spec.SpecCase[R, E, Spec[R, E]] =
          spec.caseValue match {
            case Spec.ExecCase(exec, spec)     => Spec.ExecCase(exec, loop(acc, spec))
            case Spec.LabeledCase(label, spec) => Spec.LabeledCase(label, loop(acc :+ label, spec))
            case Spec.ScopedCase(scoped)       => Spec.ScopedCase(scoped.map(loop(acc, _)))
            case Spec.MultipleCase(specs)      => Spec.MultipleCase(specs.map(loop(acc, _)))
            case Spec.TestCase(test, annotations) =>
              Spec.TestCase(
                test @@ ZIO.withLogSpans(LogSpan(acc.toContiguous.mkString(" / "), java.time.Instant.now().toEpochMilli) :: Nil),
                annotations,
              )
          }

        Spec(res)
      }

      override def some[R, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] = loop(Growable.empty, spec)
    }

}
