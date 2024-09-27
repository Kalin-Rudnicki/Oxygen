package oxygen.predef

object test {
  export oxygen.predef.core.*
  export oxygen.test.{OxygenSpec, OxygenSpecDefault}
  export oxygen.test.OAssertions.*
  export zio.test.{assert, assertCompletes, assertCompletesZIO, assertTrue, assertZIO}
  export zio.test.Assertion.*
}
