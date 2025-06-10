package oxygen.quoted.companion

import oxygen.quoted.*
import scala.annotation.experimental
import scala.quoted.*

final class SymbolCompanion(using quotes: Quotes) {

  /**
    * Symbol of the definition that encloses the current splicing context.
    *
    *  For example, the following call to `spliceOwner` would return the symbol `x`.
    *  ```scala sc:nocompile
    *  val x = ${ ... Symbol.spliceOwner ... }
    *  ```
    *
    *  For a macro splice, it is the symbol of the definition where the macro expansion happens.
    */
  def spliceOwner: Symbol =
    Symbol.wrap(quotes.reflect.Symbol.spliceOwner)

  /** Get package symbol if package is either defined in current compilation run or present on classpath. */
  def requiredPackage(path: String): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.requiredPackage(path))

  /** Get class symbol if class is either defined in current compilation run or present on classpath. */
  def requiredClass(path: String): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.requiredClass(path))

  /** Get module symbol if module is either defined in current compilation run or present on classpath. */
  def requiredModule(path: String): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.requiredModule(path))

  /** Get method symbol if method is either defined in current compilation run or present on classpath. Throws if the method has an overload. */
  def requiredMethod(path: String): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.requiredMethod(path))

  /** The class Symbol of a global class definition */
  def classSymbol(fullName: String): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.classSymbol(fullName))

  /**
    * Generates a new class symbol for a class with a public parameterless constructor.
    *  For more settings, look to the other newClass methods.
    *
    *  Example usage:
    *  ```
    *  val name: String = "myClass"
    *  val parents = List(TypeTree.of[Object], TypeTree.of[Foo])
    *  def decls(cls: Symbol): List[Symbol] =
    *    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
    *
    *  val cls = Symbol.newClass(Symbol.spliceOwner, name, parents = parents.map(_.tpe), decls, selfType = None)
    *  val fooSym = cls.declaredMethod("foo").head
    *
    *  val fooDef = DefDef(fooSym, argss => Some('{println(s"Calling foo")}.asTerm))
    *  val clsDef = ClassDef(cls, parents, body = List(fooDef))
    *  val newCls = Typed(Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), Nil), TypeTree.of[Foo])
    *
    *  Block(List(clsDef), newCls).asExprOf[Foo]
    *  ```
    *  constructs the equivalent to
    *   ```
    *  '{
    *    class myClass() extends Object with Foo {
    *      def foo(): Unit = println("Calling foo")
    *    }
    *    new myClass(): Foo
    *  }
    *  ```
    *
    *  @param owner The owner of the class
    *  @param name The name of the class
    *  @param parents The parent classes of the class. The first parent must not be a trait.
    *  @param decls The member declarations of the class provided the symbol of this class
    *  @param selfType The self type of the class if it has one
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the ClassDef constructor.
    *
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  @experimental
  def newClass(owner: Symbol, name: String, parents: List[TypeRepr], decls: Symbol => List[Symbol], selfType: Option[TypeRepr]): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newClass(owner.unwrapWithin, name, parents.map(_.unwrapWithin), s => decls(Symbol.wrap(s)).map(_.unwrapWithin), selfType.map(_.unwrapWithin)))

  /**
    * Generates a new class symbol for a class with a public single term clause constructor.
    *
    *  Example usage:
    *  ```
    *  val name = "myClass"
    *  def decls(cls: Symbol): List[Symbol] =
    *    List(Symbol.newMethod(cls, "foo", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit])))
    *  val parents = List(TypeTree.of[Object])
    *  val cls = Symbol.newClass(
    *    Symbol.spliceOwner,
    *    name,
    *    parents = _ => parents.map(_.tpe),
    *    decls,
    *    selfType = None,
    *    clsFlags = Flags.EmptyFlags,
    *    Symbol.noSymbol,
    *    List(("idx", TypeRepr.of[Int]), ("str", TypeRepr.of[String]))
    *  )
    *
    *  val fooSym = cls.declaredMethod("foo").head
    *  val idxSym = cls.fieldMember("idx")
    *  val strSym = cls.fieldMember("str")
    *  val fooDef = DefDef(fooSym, argss =>
    *    Some('{println(s"Foo method call with (${${Ref(idxSym).asExpr}}, ${${Ref(strSym).asExpr}})")}.asTerm)
    *  )
    *  val clsDef = ClassDef(cls, parents, body = List(fooDef))
    *  val newCls = Apply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List('{0}.asTerm, '{string}.asTerm))
    *
    *  Block(List(clsDef), Apply(Select(newCls, cls.methodMember("foo")(0)), Nil)).asExprOf[Unit]
    *  ```
    *  construct the equivalent to
    *  ```
    *  '{
    *    class myClass(idx: Int, str: String) extends Object {
    *      def foo() =
    *        println(s"Foo method call with $idx, $str")
    *    }
    *    new myClass(0, "string").foo()
    *  }
    *  ```
    *  @param owner The owner of the class
    *  @param name The name of the class
    *  @param parents Function returning the parent classes of the class. The first parent must not be a trait.
    *  Takes the constructed class symbol as an argument. Calling `cls.typeRef.asType` as part of this function will lead to cyclic reference errors.
    *  @param clsFlags extra flags with which the class symbol should be constructed.
    *  @param clsPrivateWithin the symbol within which this new class symbol should be private. May be noSymbol.
    *  @param conParams constructor parameter pairs of names and types.
    *
    *  Parameters assigned by the constructor can be obtained via `classSymbol.memberField`.
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the ClassDef constructor.
    *
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  @experimental
  def newClass(
      owner: Symbol,
      name: String,
      parents: Symbol => List[TypeRepr],
      decls: Symbol => List[Symbol],
      selfType: Option[TypeRepr],
      clsFlags: Flags,
      clsPrivateWithin: Symbol,
      conParams: List[(String, TypeRepr)],
  ): Symbol =
    Symbol.wrap(
      quotes.reflect.Symbol.newClass(
        owner.unwrapWithin,
        name,
        s => parents(Symbol.wrap(s)).map(_.unwrapWithin),
        s => decls(Symbol.wrap(s)).map(_.unwrapWithin),
        selfType.map(_.unwrapWithin),
        clsFlags.unwrapWithin,
        clsPrivateWithin.unwrapWithin,
        conParams.map((n, t) => (n, t.unwrapWithin)),
      ),
    )

  /**
    * Generates a new class symbol with a constructor of the shape signified by a passed PolyOrMethod parameter.
    *
    *  Example usage:
    *  ```
    *  val name = "myClass"
    *  def decls(cls: Symbol): List[Symbol] =
    *    List(Symbol.newMethod(cls, "getParam", MethodType(Nil)(_ => Nil, _ => cls.typeMember("T").typeRef)))
    *  val conMethodType =
    *    (classType: TypeRepr) => PolyType(List("T"))(_ => List(TypeBounds.empty), polyType =>
    *      MethodType(List("param"))((_: MethodType) => List(polyType.param(0)), (_: MethodType) =>
    *        AppliedType(classType, List(polyType.param(0)))
    *      )
    *    )
    *  val cls = Symbol.newClass(
    *    Symbol.spliceOwner,
    *    name,
    *    parents = _ => List(TypeRepr.of[Object]),
    *    decls,
    *    selfType = None,
    *    clsFlags = Flags.EmptyFlags,
    *    clsPrivateWithin = Symbol.noSymbol,
    *    clsAnnotations = Nil,
    *    conMethodType,
    *    conFlags = Flags.EmptyFlags,
    *    conPrivateWithin = Symbol.noSymbol,
    *    conParamFlags = List(List(Flags.EmptyFlags), List(Flags.EmptyFlags)),
    *    conParamPrivateWithins = List(List(Symbol.noSymbol), List(Symbol.noSymbol))
    *  )
    *
    *  val getParamSym = cls.declaredMethod("getParam").head
    *  def getParamRhs(): Option[Term] =
    *    val paramValue = This(cls).select(cls.fieldMember("param")).asExpr
    *    Some('{ println("Calling getParam"); $paramValue }.asTerm)
    *  val getParamDef = DefDef(getParamSym, _ => getParamRhs())
    *
    *  val clsDef = ClassDef(cls, List(TypeTree.of[Object]), body = List(getParamDef))
    *  val newCls =
    *    Apply(
    *      Select(
    *        Apply(
    *          TypeApply(Select(New(TypeIdent(cls)), cls.primaryConstructor), List(TypeTree.of[String])),
    *          List(Expr("test").asTerm)
    *        ),
    *        cls.methodMember("getParam").head
    *      ),
    *      Nil
    *    )
    *
    *  Block(List(clsDef), newCls).asExpr
    *  ```
    *  constructs the equivalent to
    *  ```
    *  '{
    *    class myClass[T](val param: T) extends Object {
    *      def getParam: T =
    *        println("Calling getParam")
    *        param
    *    }
    *    new myClass[String]("test").getParam()
    *  }
    *  ```
    *
    * @param owner The owner of the class
    * @param name The name of the class
    * @param parents Function returning the parent classes of the class. The first parent must not be a trait
    * Takes the constructed class symbol as an argument. Calling `cls.typeRef.asType` as part of this function will lead to cyclic reference errors.
    * @param decls The member declarations of the class provided the symbol of this class
    * @param selfType The self type of the class if it has one
    * @param clsFlags extra flags with which the class symbol should be constructed. Can be `Private` | `Protected` | `PrivateLocal` | `Local` | `Final` | `Trait` | `Abstract` | `Open`
    * @param clsPrivateWithin the symbol within which this new class symbol should be private. May be noSymbol
    * @param clsAnnotations annotations of the class
    * @param conMethodType Function returning MethodOrPoly type representing the type of the constructor.
    * Takes the result type as parameter which must be returned from the innermost MethodOrPoly and have type parameters applied if those are used.
    * PolyType may only represent the first clause of the constructor.
    * @param conFlags extra flags with which the constructor symbol should be constructed. Can be `Synthetic` | `Method` | `Private` | `Protected` | `PrivateLocal` | `Local`
    * @param conPrivateWithin the symbol within which the constructor for this new class symbol should be private. May be noSymbol.
    * @param conParamFlags extra flags with which the constructor parameter symbols should be constructed. Must match the shape of `conMethodType`.
    * For type parameters those can be `Param` | `Deferred` | `Private` | `PrivateLocal` | `Local`.
    * For term parameters those can be `ParamAccessor` | `Private` | `Protected` | `PrivateLocal` | `Local`
    * @param conParamPrivateWithins the symbols within which the constructor parameters should be private. Must match the shape of `conMethodType`. Can consist of noSymbol.
    *
    *  Term and type parameters assigned by the constructor can be obtained via `classSymbol.memberField`/`classSymbol.memberType`.
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the ClassDef constructor.
    *
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  @experimental
  def newClass(
      owner: Symbol,
      name: String,
      parents: Symbol => List[TypeRepr],
      decls: Symbol => List[Symbol],
      selfType: Option[TypeRepr],
      clsFlags: Flags,
      clsPrivateWithin: Symbol,
      clsAnnotations: List[Term],
      conMethodType: TypeRepr => MethodOrPoly,
      conFlags: Flags,
      conPrivateWithin: Symbol,
      conParamFlags: List[List[Flags]],
      conParamPrivateWithins: List[List[Symbol]],
  ): Symbol =
    Symbol.wrap(
      quotes.reflect.Symbol.newClass(
        owner.unwrapWithin,
        name,
        s => parents(Symbol.wrap(s)).map(_.unwrapWithin),
        s => decls(Symbol.wrap(s)).map(_.unwrapWithin),
        selfType.map(_.unwrapWithin),
        clsFlags.unwrapWithin,
        clsPrivateWithin.unwrapWithin,
        clsAnnotations.map(_.unwrapWithin),
        t => conMethodType(TypeRepr.wrap(t)).unwrapWithin,
        conFlags.unwrapWithin,
        conPrivateWithin.unwrapWithin,
        conParamFlags.map(_.map(_.unwrapWithin)),
        conParamPrivateWithins.map(_.map(_.unwrapWithin)),
      ),
    )

  /**
    * Generates a new module symbol with an associated module class symbol,
    *  this is equivalent to an `object` declaration in source code.
    *  This method returns the module symbol. The module class can be accessed calling `moduleClass` on this symbol.
    *
    *  Example usage:
    *  ```scala
    *  //{
    *  given Quotes = ???
    *  import quotes.reflect.*
    *  //}
    *  val moduleName: String = Symbol.freshName("MyModule")
    *  val parents = List(TypeTree.of[Object])
    *  def decls(cls: Symbol): List[Symbol] =
    *    List(Symbol.newMethod(cls, "run", MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Unit]), Flags.EmptyFlags, Symbol.noSymbol))
    *
    *  val mod = Symbol.newModule(Symbol.spliceOwner, moduleName, Flags.EmptyFlags, Flags.EmptyFlags, _ => parents.map(_.tpe), decls, Symbol.noSymbol)
    *  val cls = mod.moduleClass
    *  val runSym = cls.declaredMethod("run").head
    *
    *  val runDef = DefDef(runSym, _ => Some('{ println("run") }.asTerm))
    *  val modDef = ClassDef.module(mod, parents, body = List(runDef))
    *
    *  val callRun = Apply(Select(Ref(mod), runSym), Nil)
    *
    *  Block(modDef.toList, callRun)
    *  ```
    *  constructs the equivalent to
    *  ```scala
    *  //{
    *  given Quotes = ???
    *  import quotes.reflect.*
    *  //}
    *  '{
    *    object MyModule$macro$1 extends Object:
    *      def run(): Unit = println("run")
    *    MyModule$macro$1.run()
    *  }
    *  ```
    *
    *  @param parent The owner of the class
    *  @param name The name of the class
    *  @param modFlags extra flags with which the module symbol should be constructed
    *  @param clsFlags extra flags with which the module class symbol should be constructed
    *  @param parents A function that takes the symbol of the module class as input and returns the parent classes of the class. The first parent must not be a trait.
    *  @param decls A function that takes the symbol of the module class as input and return the symbols of its declared members
    *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to `ClassDef.module`.
    *
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    *
    *  @syntax markdown
    */
  @experimental
  def newModule(
      owner: Symbol,
      name: String,
      modFlags: Flags,
      clsFlags: Flags,
      parents: Symbol => List[TypeRepr],
      decls: Symbol => List[Symbol],
      privateWithin: Symbol,
  ): Symbol =
    Symbol.wrap(
      quotes.reflect.Symbol.newModule(
        owner.unwrapWithin,
        name,
        modFlags.unwrapWithin,
        clsFlags.unwrapWithin,
        s => parents(Symbol.wrap(s)).map(_.unwrapWithin),
        s => decls(Symbol.wrap(s)).map(_.unwrapWithin),
        privateWithin.unwrapWithin,
      ),
    )

  /**
    * Generates a new method symbol with the given parent, name and type.
    *
    *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
    *
    *  @param parent The owner of the method
    *  @param name The name of the method
    *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the DefDef constructor.
    *
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  def newMethod(parent: Symbol, name: String, tpe: TypeRepr): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newMethod(parent.unwrapWithin, name, tpe.unwrapWithin))

  /**
    * Works as the other newMethod, but with additional parameters.
    *
    *  To define a member method of a class, use the `newMethod` within the `decls` function of `newClass`.
    *
    *  @param parent The owner of the method
    *  @param name The name of the method
    *  @param tpe The type of the method (MethodType, PolyType, ByNameType)
    *  @param flags extra flags to with which the symbol should be constructed. `Method` flag will be added. Can be `Private | Protected | Override | Deferred | Final | Method | Implicit | Given | Local | JavaStatic | Synthetic | Artifact`
    *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
    */
  def newMethod(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newMethod(parent.unwrapWithin, name, tpe.unwrapWithin, flags.unwrapWithin, privateWithin.unwrapWithin))

  /**
    * Generates a new val/var/lazy val symbol with the given parent, name and type.
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the ValDef constructor.
    *
    *  Note: Also see ValDef.let
    *
    *  @param parent The owner of the val/var/lazy val
    *  @param name The name of the val/var/lazy val
    *  @param tpe The type of the val/var/lazy val
    *  @param flags extra flags to with which the symbol should be constructed. Can be `Private | Protected | Override | Deferred | Final | Param | Implicit | Lazy | Mutable | Local | ParamAccessor | Module | Package | Case | CaseAccessor | Given | Enum | JavaStatic | Synthetic | Artifact`
    *  @param privateWithin the symbol within which this new method symbol should be private. May be noSymbol.
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  def newVal(parent: Symbol, name: String, tpe: TypeRepr, flags: Flags, privateWithin: Symbol): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newVal(parent.unwrapWithin, name, tpe.unwrapWithin, flags.unwrapWithin, privateWithin.unwrapWithin))

  /**
    * Generates a pattern bind symbol with the given parent, name and type.
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the BindDef constructor.
    *
    *  @param parent The owner of the binding
    *  @param name The name of the binding
    *  @param flags extra flags to with which the symbol should be constructed. `Case` flag will be added. Can be `Case`
    *  @param tpe The type of the binding
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  def newBind(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newBind(parent.unwrapWithin, name, flags.unwrapWithin, tpe.unwrapWithin))

  /**
    * Generate a new type symbol for a type alias with the given parent, name and type
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the TypeDef constructor.
    *
    *  @param parent The owner of the type
    *  @param name The name of the type
    *  @param flags extra flags to with which symbol can be constructed. Can be `Private` | `Protected` | `Override` | `Final` | `Infix` | `Local`
    *  @param tpe The rhs the type alias
    *  @param privateWithin the symbol within which this new type symbol should be private. May be noSymbol.
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  @experimental
  def newTypeAlias(parent: Symbol, name: String, flags: Flags, tpe: TypeRepr, privateWithin: Symbol): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newTypeAlias(parent.unwrapWithin, name, flags.unwrapWithin, tpe.unwrapWithin, privateWithin.unwrapWithin))

  /**
    * Generate a new type symbol for a type bounds with the given parent, name and type
    *
    *  This symbol starts without an accompanying definition.
    *  It is the meta-programmer's responsibility to provide exactly one corresponding definition by passing
    *  this symbol to the TypeDef constructor.
    *
    *  @param parent The owner of the type
    *  @param name The name of the type
    *  @param flags extra flags to with which symbol can be constructed. `Deferred` flag will be added. Can be `Private` | `Protected` | `Override` | `Deferred` | `Final` | `Infix` | `Local`
    *  @param tpe The bounds of the type
    *  @param privateWithin the symbol within which this new type symbol should be private. May be noSymbol.
    *  @note As a macro can only splice code into the point at which it is expanded, all generated symbols must be
    *        direct or indirect children of the reflection context's owner.
    */
  @experimental
  def newBoundedType(parent: Symbol, name: String, flags: Flags, tpe: TypeBounds, privateWithin: Symbol): Symbol =
    Symbol.wrap(quotes.reflect.Symbol.newBoundedType(parent.unwrapWithin, name, flags.unwrapWithin, tpe.unwrapWithin, privateWithin.unwrapWithin))

  /** Definition not available */
  def noSymbol: Symbol =
    Symbol.wrap(quotes.reflect.Symbol.noSymbol)

  /**
    * A fresh name for class or member symbol names.
    *
    *  Fresh names are constructed using the following format `prefix + "$macro$" + freshIndex`.
    *  The `freshIndex` are unique within the current source file.
    *
    *  Examples: See `scala.annotation.MacroAnnotation`
    *
    *  @param prefix Prefix of the fresh name
    */
  @experimental
  def freshName(prefix: String): String =
    quotes.reflect.Symbol.freshName(prefix)

  // =====| Added |=====

  def tupleClass(arity: Int): Symbol =
    Symbol.wrap(quotes.reflect.defn.TupleClass(arity))

}
