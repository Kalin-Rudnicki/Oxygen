package oxygen.test

import oxygen.core.collection.Growable
import oxygen.zio.*
import oxygen.zio.logging.LogConfig
import oxygen.zio.syntax.log.*
import zio.*
import zio.compat.logOps
import zio.test.*

object OxygenAspects {

  extension [_R](self: FiberRefModificationR[_R]) {

    /**
      * Will apply the following fiber-ref-modification on a per-test basis.
      */
    def toTestAspectPerTest: TestAspect.PerTest.AtLeastR[_R] =
      new TestAspect.PerTest.AtLeastR[_R] {
        override def perTest[R <: _R, E](test: ZIO[R, TestFailure[E], TestSuccess])(implicit trace: Trace): ZIO[R, TestFailure[E], TestSuccess] =
          test @@ self
      }

    /**
      * Will apply the following fiber-ref-modification in a scoped manner to the whole spec.
      */
    def toTestAspectGlobal: TestAspectAtLeastR[_R] =
      new TestAspectAtLeastR[_R] {
        override def some[R <: _R, E](spec: Spec[R, E])(implicit trace: Trace): Spec[R, E] =
          Spec { Spec.ScopedCase { self.setScoped.as(spec) } }
      }

  }

  def usingConfig(config: LogConfig): TestAspectAtLeastR[Any] =
    LogConfig.usingConfig(config).toTestAspectGlobal

  def withMinLogLevel(level: LogLevel): TestAspectAtLeastR[Any] =
    logOps.withMinLogLevel(level).toTestAspectGlobal

  val silentLogging: TestAspectAtLeastR[Any] =
    withMinLogLevel(LogLevel.None)

  val withTestAsLogSpan: TestAspect[Nothing, Any, Nothing, Any] =
    new TestAspect[Nothing, Any, Nothing, Any] {

      def loop[R, E](acc: Growable[String], spec: Spec[R, E])(using trace: Trace): Spec[R, E] = {
        val res: Spec.SpecCase[R, E, Spec[R, E]] =
          spec.caseValue match {
            case Spec.ExecCase(exec, spec)        => Spec.ExecCase(exec, loop(acc, spec))
            case Spec.LabeledCase(label, spec)    => Spec.LabeledCase(label, loop(acc :+ label, spec))
            case Spec.ScopedCase(scoped)          => Spec.ScopedCase(scoped.map(loop(acc, _)))
            case Spec.MultipleCase(specs)         => Spec.MultipleCase(specs.map(loop(acc, _)))
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
