package oxygen.predef

object test {
  // from other predefs
  export oxygen.predef.core.*
  // TODO (KR) : export oxygen.predef.zio.*
  // from oxygen-test
  export oxygen.test.{OxygenSpec, OxygenSpecDefault}
  export oxygen.test.OAssertions.*
  export zio.internal.stacktracer.SourceLocation
  export zio.test.{assert, assertCompletes, assertCompletesZIO, assertTrue, assertZIO}
  export zio.test.Assertion.*
}
