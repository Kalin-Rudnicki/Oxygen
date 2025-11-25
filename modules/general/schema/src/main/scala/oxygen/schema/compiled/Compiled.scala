package oxygen.schema.compiled

import oxygen.core.typeclass.Monad
import oxygen.schema.{JsonSchema, PlainTextSchema}

sealed trait Compiled[+A] {

  private[Compiled] def compileInternal(compiler: SchemaCompiler): (A, SchemaCompiler)

  final def compiled: Compiled.Output[A] = {
    val (a, compiler) = compileInternal(SchemaCompiler.initial)
    Compiled.Output(a, compiler.finalizedSchemas)
  }

  /**
    * Oxygen schema keeps track of source line numbers in some cases to help differentiate where a schema came from.
    * If you are serializing compiled schemas for documentation purposes or for compatibility checking, you may want to remove line numbers.
    * Otherwise, line numbers may cause unnecessary diffs or changes in the serialized output.
    */
  final def compiledWithoutLineNos: Compiled.Output[A] = compiled.withoutLineNos

  final def map[B](f: A => B): Compiled[B] = Compiled.internal.Mapped(this, f)
  final def flatMap[B](f: A => Compiled[B]): Compiled[B] = Compiled.internal.FlatMapped(this, f)

}
object Compiled {

  final case class Output[+A](value: A, schemas: RawCompiledSchemas) {
    def withoutLineNos: Output[A] = Output(value, schemas.withoutLineNos)
  }

  def succeed[A](value: A): Compiled[A] = Compiled.internal.Succeed(value)

  def plain(schema: PlainTextSchema[?]): Compiled[CompiledSchemaRef.PlainLike] = Compiled.internal.CompilePlain(schema)
  def usingPlain[A: PlainTextSchema as schema]: Compiled[CompiledSchemaRef.PlainLike] = Compiled.internal.CompilePlain(schema)

  def json(schema: JsonSchema[?]): Compiled[CompiledSchemaRef.JsonLike] = Compiled.internal.CompileJson(schema)
  def usingJson[A: JsonSchema as schema]: Compiled[CompiledSchemaRef.JsonLike] = Compiled.internal.CompileJson(schema)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given monad: Monad[Compiled] =
    new Monad[Compiled] {

      override def map[A, B](self: Compiled[A])(f: A => B): Compiled[B] =
        self.map(f)

      override def pure[A](self: A): Compiled[A] =
        Compiled.succeed(self)

      override def ap[A, B](f: Compiled[A => B])(self: Compiled[A]): Compiled[B] =
        for {
          f <- f
          self <- self
        } yield f(self)

      override def flatMap[A, B](self: Compiled[A])(f: A => Compiled[B]): Compiled[B] =
        self.flatMap(f)

    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object internal {

    final case class Succeed[+A](value: A) extends Compiled[A] {
      override private[Compiled] def compileInternal(compiler: SchemaCompiler): (A, SchemaCompiler) = (value, compiler)
    }

    final case class CompilePlain(schema: PlainTextSchema[?]) extends Compiled[CompiledSchemaRef.PlainLike] {
      override private[Compiled] def compileInternal(compiler: SchemaCompiler): (CompiledSchemaRef.PlainLike, SchemaCompiler) = compiler.compilePlain(schema)
    }

    final case class CompileJson(schema: JsonSchema[?]) extends Compiled[CompiledSchemaRef.JsonLike] {
      override private[Compiled] def compileInternal(compiler: SchemaCompiler): (CompiledSchemaRef.JsonLike, SchemaCompiler) = compiler.compileJson(schema)
    }

    final case class Mapped[A, B](a: Compiled[A], f: A => B) extends Compiled[B] {
      override private[Compiled] def compileInternal(compiler: SchemaCompiler): (B, SchemaCompiler) = {
        val (v1, v2) = a.compileInternal(compiler)
        (f(v1), v2)
      }
    }

    final case class FlatMapped[A, B](a: Compiled[A], f: A => Compiled[B]) extends Compiled[B] {
      override private[Compiled] def compileInternal(compiler: SchemaCompiler): (B, SchemaCompiler) = {
        val (v1, v2) = a.compileInternal(compiler)
        f(v1).compileInternal(v2)
      }
    }

  }

}
