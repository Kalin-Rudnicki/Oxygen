package oxygen.core.typeclass

trait Zip3[In1, In2, In3] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3): Out
  def unzip(out: Out): (In1, In2, In3)
}
object Zip3 {

  type Out[In1, In2, In3, O] = Zip3[In1, In2, In3] { type Out = O }

  inline def apply[In1, In2, In3](implicit zip: Zip3[In1, In2, In3]): zip.type = zip

  final class Instance[In1, In2, Out1, In3, Out2](
      zip1: Zip.Out[In1, In2, Out1],
      zip2: Zip.Out[Out1, In3, Out2],
  ) extends Zip3[In1, In2, In3] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3): Out2 =
      zip2.zip(zip1.zip(in1, in2), in3)

    override def unzip(out: Out2): (In1, In2, In3) = {
      val (tmp, i3) = zip2.unzip(out)
      val (i1, i2) = zip1.unzip(tmp)
      (i1, i2, i3)
    }

  }

  implicit def make[In1, In2, Out1, In3, Out2](implicit
      zip1: Zip.Out[In1, In2, Out1],
      zip2: Zip.Out[Out1, In3, Out2],
  ): Zip3.Out[In1, In2, In3, Out2] =
    new Zip3.Instance[In1, In2, Out1, In3, Out2](zip1, zip2)

}

trait Zip4[In1, In2, In3, In4] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4): Out
  def unzip(out: Out): (In1, In2, In3, In4)
}
object Zip4 {

  type Out[In1, In2, In3, In4, O] = Zip4[In1, In2, In3, In4] { type Out = O }

  inline def apply[In1, In2, In3, In4](implicit zip: Zip4[In1, In2, In3, In4]): zip.type = zip

  final class Instance[In1, In2, In3, Out1, In4, Out2](
      zip1: Zip3.Out[In1, In2, In3, Out1],
      zip2: Zip.Out[Out1, In4, Out2],
  ) extends Zip4[In1, In2, In3, In4] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3), in4)

    override def unzip(out: Out2): (In1, In2, In3, In4) = {
      val (tmp, i4) = zip2.unzip(out)
      val (i1, i2, i3) = zip1.unzip(tmp)
      (i1, i2, i3, i4)
    }

  }

  implicit def make[In1, In2, In3, Out1, In4, Out2](implicit
      zip1: Zip3.Out[In1, In2, In3, Out1],
      zip2: Zip.Out[Out1, In4, Out2],
  ): Zip4.Out[In1, In2, In3, In4, Out2] =
    new Zip4.Instance[In1, In2, In3, Out1, In4, Out2](zip1, zip2)

}

trait Zip5[In1, In2, In3, In4, In5] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5)
}
object Zip5 {

  type Out[In1, In2, In3, In4, In5, O] = Zip5[In1, In2, In3, In4, In5] { type Out = O }

  inline def apply[In1, In2, In3, In4, In5](implicit zip: Zip5[In1, In2, In3, In4, In5]): zip.type = zip

  final class Instance[In1, In2, In3, In4, Out1, In5, Out2](
      zip1: Zip4.Out[In1, In2, In3, In4, Out1],
      zip2: Zip.Out[Out1, In5, Out2],
  ) extends Zip5[In1, In2, In3, In4, In5] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3, in4), in5)

    override def unzip(out: Out2): (In1, In2, In3, In4, In5) = {
      val (tmp, i5) = zip2.unzip(out)
      val (i1, i2, i3, i4) = zip1.unzip(tmp)
      (i1, i2, i3, i4, i5)
    }

  }

  implicit def make[In1, In2, In3, In4, Out1, In5, Out2](implicit
      zip1: Zip4.Out[In1, In2, In3, In4, Out1],
      zip2: Zip.Out[Out1, In5, Out2],
  ): Zip5.Out[In1, In2, In3, In4, In5, Out2] =
    new Zip5.Instance[In1, In2, In3, In4, Out1, In5, Out2](zip1, zip2)

}

trait Zip6[In1, In2, In3, In4, In5, In6] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6)
}
object Zip6 {

  type Out[In1, In2, In3, In4, In5, In6, O] = Zip6[In1, In2, In3, In4, In5, In6] { type Out = O }

  inline def apply[In1, In2, In3, In4, In5, In6](implicit zip: Zip6[In1, In2, In3, In4, In5, In6]): zip.type = zip

  final class Instance[In1, In2, In3, In4, In5, Out1, In6, Out2](
      zip1: Zip5.Out[In1, In2, In3, In4, In5, Out1],
      zip2: Zip.Out[Out1, In6, Out2],
  ) extends Zip6[In1, In2, In3, In4, In5, In6] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3, in4, in5), in6)

    override def unzip(out: Out2): (In1, In2, In3, In4, In5, In6) = {
      val (tmp, i6) = zip2.unzip(out)
      val (i1, i2, i3, i4, i5) = zip1.unzip(tmp)
      (i1, i2, i3, i4, i5, i6)
    }

  }

  implicit def make[In1, In2, In3, In4, In5, Out1, In6, Out2](implicit
      zip1: Zip5.Out[In1, In2, In3, In4, In5, Out1],
      zip2: Zip.Out[Out1, In6, Out2],
  ): Zip6.Out[In1, In2, In3, In4, In5, In6, Out2] =
    new Zip6.Instance[In1, In2, In3, In4, In5, Out1, In6, Out2](zip1, zip2)

}

trait Zip7[In1, In2, In3, In4, In5, In6, In7] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7)
}
object Zip7 {

  type Out[In1, In2, In3, In4, In5, In6, In7, O] = Zip7[In1, In2, In3, In4, In5, In6, In7] { type Out = O }

  inline def apply[In1, In2, In3, In4, In5, In6, In7](implicit zip: Zip7[In1, In2, In3, In4, In5, In6, In7]): zip.type = zip

  final class Instance[In1, In2, In3, In4, In5, In6, Out1, In7, Out2](
      zip1: Zip6.Out[In1, In2, In3, In4, In5, In6, Out1],
      zip2: Zip.Out[Out1, In7, Out2],
  ) extends Zip7[In1, In2, In3, In4, In5, In6, In7] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3, in4, in5, in6), in7)

    override def unzip(out: Out2): (In1, In2, In3, In4, In5, In6, In7) = {
      val (tmp, i7) = zip2.unzip(out)
      val (i1, i2, i3, i4, i5, i6) = zip1.unzip(tmp)
      (i1, i2, i3, i4, i5, i6, i7)
    }

  }

  implicit def make[In1, In2, In3, In4, In5, In6, Out1, In7, Out2](implicit
      zip1: Zip6.Out[In1, In2, In3, In4, In5, In6, Out1],
      zip2: Zip.Out[Out1, In7, Out2],
  ): Zip7.Out[In1, In2, In3, In4, In5, In6, In7, Out2] =
    new Zip7.Instance[In1, In2, In3, In4, In5, In6, Out1, In7, Out2](zip1, zip2)

}

trait Zip8[In1, In2, In3, In4, In5, In6, In7, In8] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7, In8)
}
object Zip8 {

  type Out[In1, In2, In3, In4, In5, In6, In7, In8, O] = Zip8[In1, In2, In3, In4, In5, In6, In7, In8] { type Out = O }

  inline def apply[In1, In2, In3, In4, In5, In6, In7, In8](implicit zip: Zip8[In1, In2, In3, In4, In5, In6, In7, In8]): zip.type = zip

  final class Instance[In1, In2, In3, In4, In5, In6, In7, Out1, In8, Out2](
      zip1: Zip7.Out[In1, In2, In3, In4, In5, In6, In7, Out1],
      zip2: Zip.Out[Out1, In8, Out2],
  ) extends Zip8[In1, In2, In3, In4, In5, In6, In7, In8] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3, in4, in5, in6, in7), in8)

    override def unzip(out: Out2): (In1, In2, In3, In4, In5, In6, In7, In8) = {
      val (tmp, i8) = zip2.unzip(out)
      val (i1, i2, i3, i4, i5, i6, i7) = zip1.unzip(tmp)
      (i1, i2, i3, i4, i5, i6, i7, i8)
    }

  }

  implicit def make[In1, In2, In3, In4, In5, In6, In7, Out1, In8, Out2](implicit
      zip1: Zip7.Out[In1, In2, In3, In4, In5, In6, In7, Out1],
      zip2: Zip.Out[Out1, In8, Out2],
  ): Zip8.Out[In1, In2, In3, In4, In5, In6, In7, In8, Out2] =
    new Zip8.Instance[In1, In2, In3, In4, In5, In6, In7, Out1, In8, Out2](zip1, zip2)

}

trait Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7, In8, In9)
}
object Zip9 {

  type Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, O] = Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] { type Out = O }

  inline def apply[In1, In2, In3, In4, In5, In6, In7, In8, In9](implicit zip: Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9]): zip.type = zip

  final class Instance[In1, In2, In3, In4, In5, In6, In7, In8, Out1, In9, Out2](
      zip1: Zip8.Out[In1, In2, In3, In4, In5, In6, In7, In8, Out1],
      zip2: Zip.Out[Out1, In9, Out2],
  ) extends Zip9[In1, In2, In3, In4, In5, In6, In7, In8, In9] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3, in4, in5, in6, in7, in8), in9)

    override def unzip(out: Out2): (In1, In2, In3, In4, In5, In6, In7, In8, In9) = {
      val (tmp, i9) = zip2.unzip(out)
      val (i1, i2, i3, i4, i5, i6, i7, i8) = zip1.unzip(tmp)
      (i1, i2, i3, i4, i5, i6, i7, i8, i9)
    }

  }

  implicit def make[In1, In2, In3, In4, In5, In6, In7, In8, Out1, In9, Out2](implicit
      zip1: Zip8.Out[In1, In2, In3, In4, In5, In6, In7, In8, Out1],
      zip2: Zip.Out[Out1, In9, Out2],
  ): Zip9.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, Out2] =
    new Zip9.Instance[In1, In2, In3, In4, In5, In6, In7, In8, Out1, In9, Out2](zip1, zip2)

}

trait Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] {
  type Out

  def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9, in10: In10): Out
  def unzip(out: Out): (In1, In2, In3, In4, In5, In6, In7, In8, In9, In10)
}
object Zip10 {

  type Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10, O] = Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] { type Out = O }

  inline def apply[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10](implicit zip: Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10]): zip.type = zip

  final class Instance[In1, In2, In3, In4, In5, In6, In7, In8, In9, Out1, In10, Out2](
      zip1: Zip9.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, Out1],
      zip2: Zip.Out[Out1, In10, Out2],
  ) extends Zip10[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10] {

    override type Out = Out2

    override def zip(in1: In1, in2: In2, in3: In3, in4: In4, in5: In5, in6: In6, in7: In7, in8: In8, in9: In9, in10: In10): Out2 =
      zip2.zip(zip1.zip(in1, in2, in3, in4, in5, in6, in7, in8, in9), in10)

    override def unzip(out: Out2): (In1, In2, In3, In4, In5, In6, In7, In8, In9, In10) = {
      val (tmp, i10) = zip2.unzip(out)
      val (i1, i2, i3, i4, i5, i6, i7, i8, i9) = zip1.unzip(tmp)
      (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10)
    }

  }

  implicit def make[In1, In2, In3, In4, In5, In6, In7, In8, In9, Out1, In10, Out2](implicit
      zip1: Zip9.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, Out1],
      zip2: Zip.Out[Out1, In10, Out2],
  ): Zip10.Out[In1, In2, In3, In4, In5, In6, In7, In8, In9, In10, Out2] =
    new Zip10.Instance[In1, In2, In3, In4, In5, In6, In7, In8, In9, Out1, In10, Out2](zip1, zip2)

}
