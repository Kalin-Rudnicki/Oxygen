package oxygen.executable

import oxygen.predef.core.*

sealed trait Executable {

  // TODO (KR) :

}
object Executable {

  trait Single extends Executable {

    // TODO (KR) :

  }

  final case class Many(options: NonEmptyList[(String, Executable)]) extends Executable {

    // TODO (KR) :

  }

}
