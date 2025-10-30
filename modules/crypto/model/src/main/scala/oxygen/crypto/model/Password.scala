package oxygen.crypto.model

import oxygen.predef.core.*

object Password {

  opaque type PlainText = String
  object PlainText {

    def wrap(password: String): Password.PlainText = password
    extension (password: Password.PlainText) def unsafeGetRawPassword: String = password

    given Show[Password.PlainText] = pass => s"Password.PlainText(${pass.unsafeGetRawPassword.map(_ => '*')})"
    given StringCodec[Password.PlainText] = StringCodec.string

  }

  opaque type Hashed = String
  object Hashed {

    def unsafeWrapPasswordHash(password: String): Password.Hashed = password
    extension (password: Password.Hashed) def getPasswordHash: String = password

    given Show[Password.Hashed] = pass => s"Password.Hashed(${pass.getPasswordHash.map(_ => '*')})"
    given StringCodec[Password.Hashed] = StringCodec.string

  }

}
