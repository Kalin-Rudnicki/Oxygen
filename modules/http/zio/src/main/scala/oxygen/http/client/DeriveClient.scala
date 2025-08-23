package oxygen.http.client

import oxygen.http.client.generic.*
import oxygen.http.core.generic.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.annotation.experimental
import scala.quoted.*
import zio.*

@experimental
trait DeriveClient[Api] {
  def client(client: Client): Api
}
object DeriveClient {

  def clientLayer[Api: {DeriveClient as der, Tag}]: URLayer[Client, Api] =
    ZLayer.fromFunction { der.client(_) }

  private[client] def derivedImpl[Api: Type](using Quotes): Expr[DeriveClient[Api]] = {
    val api: ApiRepr[Api] = ApiRepr.derive[Api]

    val newClassSym: Symbol =
      Symbol.newClass(
        owner = Symbol.spliceOwner,
        name = s"${api.typeRepr.typeSymbol.name}_Derived",
        parents = List(TypeRepr.of[Object], api.typeRepr),
        decls = p =>
          api.routes.toList.map { r =>
            Symbol.newMethod(p, r.defDef.name, r.methodType, Flags.Override, Symbol.noSymbol)
          },
        selfType = None,
      )

    val endpoints: ArraySeq[EndpointRepr[Api]] = api.routes.map(EndpointRepr[Api](_, newClassSym))

    def makeClassDef(clientExpr: Expr[Client]): ClassDef =
      ClassDef.companion.apply(
        cls = newClassSym,
        parents = List(TypeTree.of[Object], TypeTree.ref(api.sym)),
        body = endpoints.map(_.toDefinition(clientExpr)).toList,
      )

    def clientImpl(clientExpr: Expr[Client]): Expr[Api] = {
      val classDef: ClassDef = makeClassDef(clientExpr)

      Block.companion
        .apply(
          classDef :: Nil,
          New.companion.apply(TypeTree.ref(newClassSym)).select(newClassSym.primaryConstructor).appliedToNone,
        )
        .asExprOf[Api]
    }

    // TODO (KR) : Use a different `Symbol.newClass` builder which allows creating a primary constructor.
    //           : Then, be able to do `Block(List(newClassDef), '{ new DeriveClient[Api} { ... } })`,
    //           : instead of needing to define the class in the `def client`.
    val deriveClientExpr: Expr[DeriveClient[Api]] =
      '{
        new DeriveClient[Api] {
          override def client(client: Client): Api = ${ clientImpl('client) }
        }
      }

    // report.errorAndAbort(deriveClientExpr.showAnsiCode)

    deriveClientExpr
  }

  inline def derived[A]: DeriveClient[A] = ${ derivedImpl[A] }

}
