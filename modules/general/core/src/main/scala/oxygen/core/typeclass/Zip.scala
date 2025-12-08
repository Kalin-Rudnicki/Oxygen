package oxygen.core.typeclass

trait Zip[In1, In2] { self =>
  type Out

  def zip(in1: In1, in2: In2): Out
  def unzip(out: Out): (In1, In2)

  final def flip: Zip.Out[In2, In1, Out] =
    new Zip[In2, In1] {
      override type Out = self.Out
      override def zip(in1: In2, in2: In1): self.Out =
        self.zip(in2, in1)
      override def unzip(out: self.Out): (In2, In1) = {
        val (in1, in2) = self.unzip(out)
        (in2, in1)
      }
    }

}
object Zip extends ZipLowPriority.LowPriority1 {

  type Out[In1, In2, O] = Zip[In1, In2] { type Out = O }

  inline def apply[In1, In2](using ev: Zip[In1, In2]): ev.type = ev

  // =====|  |=====

  given zipUnitId: [In2] => Zip.Out[Unit, In2, In2] = new Zip.ZipUnitId[In2]

  // =====|  |=====

  final class ZipIdUnit[A] extends Zip[A, Unit] {
    override type Out = A
    override def zip(in1: A, in2: Unit): A = in1
    override def unzip(out: A): (A, Unit) = (out, ())
  }

  final class ZipUnitId[A] extends Zip[Unit, A] {
    override type Out = A
    override def zip(in1: Unit, in2: A): A = in2
    override def unzip(out: A): (Unit, A) = ((), out)
  }

  final class ZipTuples[In1 <: Tuple, In2 <: Tuple](in1Size: Int) extends Zip[In1, In2] {
    override type Out = Tuple.Concat[In1, In2]
    override def zip(in1: In1, in2: In2): Tuple.Concat[In1, In2] = in1 ++ in2
    override def unzip(out: Tuple.Concat[In1, In2]): (In1, In2) = out.splitAt(in1Size).asInstanceOf[(In1, In2)]
  }

  final class ZipIdTuple[In1, In2 <: Tuple] extends Zip[In1, In2] {
    override type Out = In1 *: In2
    override def zip(in1: In1, in2: In2): In1 *: In2 = in1 *: in2
    override def unzip(out: In1 *: In2): (In1, In2) = (out.head, out.tail)
  }

  final class ZipTupleId[In1 <: Tuple, In2] extends Zip[In1, In2] {
    override type Out = Tuple.Append[In1, In2]
    override def zip(in1: In1, in2: In2): Tuple.Append[In1, In2] = in1 :* in2
    override def unzip(out: Tuple.Append[In1, In2]): (In1, In2) = (out.init, out.last).asInstanceOf[(In1, In2)]
  }

  final class ZipIdId[In1, In2] extends Zip[In1, In2] {
    override type Out = (In1, In2)
    override def zip(in1: In1, in2: In2): Out = (in1, in2)
    override def unzip(out: Out): (In1, In2) = out
  }

}

object ZipLowPriority {

  trait LowPriority1 extends LowPriority2 {
    given zipIdUnit: [In1] => Zip.Out[In1, Unit, In1] = new Zip.ZipIdUnit[In1]
  }

  trait LowPriority2 extends LowPriority3 {
    given zipTuples: [In1 <: Tuple, In2 <: Tuple] => (in1Size: ValueOf[Tuple.Size[In1]]) => Zip.Out[In1, In2, Tuple.Concat[In1, In2]] =
      new Zip.ZipTuples[In1, In2](in1Size.value)
  }

  trait LowPriority3 extends LowPriority4 {
    given zipIdTuple: [In1, In2 <: Tuple] => Zip.Out[In1, In2, In1 *: In2] = new Zip.ZipIdTuple[In1, In2]
    given zipTupleId: [In1 <: Tuple, In2] => Zip.Out[In1, In2, Tuple.Append[In1, In2]] = new Zip.ZipTupleId[In1, In2]
  }

  trait LowPriority4 {
    given zipIdId: [In1, In2] => Zip.Out[In1, In2, (In1, In2)] = new Zip.ZipIdId[In1, In2]
  }

}
