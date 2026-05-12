package oxygen.core.collection.mutable

import oxygen.predef.test.*

object RingBufferSpec extends OxygenSpecDefault {

  override def testSpec: TestSpec =
    suite("RingBufferSpec")(
      test("grows") {
        val buffer: RingBuffer[Int] = new RingBuffer[Int](4)
        val u1 = buffer.used
        val a1 = buffer.allocated

        buffer.append(1)
        buffer.append(2)
        val u2 = buffer.used
        val a2 = buffer.allocated

        buffer.append(3)
        buffer.append(4)
        val u3 = buffer.used
        val a3 = buffer.allocated

        buffer.append(5)
        buffer.append(6)
        val u4 = buffer.used
        val a4 = buffer.allocated

        val p1 = buffer.popHead()
        val p2 = buffer.popHead()
        val p3 = buffer.popHead()
        val p4 = buffer.popHead()
        val p5 = buffer.popHead()
        val p6 = buffer.popHead()
        val p7 = buffer.popHead()
        val u5 = buffer.used
        val a5 = buffer.allocated

        assertTrue(
          u1 == 0,
          a1 == 4,
          u2 == 2,
          a2 == 4,
          u3 == 4,
          a3 == 4,
          u4 == 6,
          a4 == 8,
          u5 == 0,
          a5 == 8,
          p1.contains(1),
          p2.contains(2),
          p3.contains(3),
          p4.contains(4),
          p5.contains(5),
          p6.contains(6),
          p7.isEmpty,
        )
      },
      test("wraps") {
        val buffer: RingBuffer[Int] = new RingBuffer[Int](4)

        buffer.append(1)
        buffer.append(2)
        buffer.append(3)
        buffer.append(4)
        val u1 = buffer.used
        val a1 = buffer.allocated

        val p1 = buffer.popHead()
        val p2 = buffer.popHead()
        val u2 = buffer.used
        val a2 = buffer.allocated

        buffer.append(5)
        buffer.append(6)
        val u3 = buffer.used
        val a3 = buffer.allocated

        val p3 = buffer.popHead()
        val p4 = buffer.popHead()
        val p5 = buffer.popHead()
        val p6 = buffer.popHead()
        val p7 = buffer.popHead()
        val u4 = buffer.used
        val a4 = buffer.allocated

        assertTrue(
          u1 == 4,
          a1 == 4,
          u2 == 2,
          a2 == 4,
          u3 == 4,
          a3 == 4,
          u4 == 0,
          a4 == 4,
          p1.contains(1),
          p2.contains(2),
          p3.contains(3),
          p4.contains(4),
          p5.contains(5),
          p6.contains(6),
          p7.isEmpty,
        )
      },
      test("wraps + grows") {
        val buffer: RingBuffer[Int] = new RingBuffer[Int](4)

        buffer.append(1)
        buffer.append(2)
        buffer.append(3)
        buffer.append(4)
        val u1 = buffer.used
        val a1 = buffer.allocated

        val p1 = buffer.popHead()
        val p2 = buffer.popHead()
        val u2 = buffer.used
        val a2 = buffer.allocated

        buffer.append(5)
        buffer.append(6)
        buffer.append(7)
        buffer.append(8)
        val u3 = buffer.used
        val a3 = buffer.allocated

        val p3 = buffer.popHead()
        val p4 = buffer.popHead()
        val p5 = buffer.popHead()
        val p6 = buffer.popHead()
        val p7 = buffer.popHead()
        val p8 = buffer.popHead()
        val p9 = buffer.popHead()
        val u4 = buffer.used
        val a4 = buffer.allocated

        assertTrue(
          u1 == 4,
          a1 == 4,
          u2 == 2,
          a2 == 4,
          u3 == 6,
          a3 == 8,
          u4 == 0,
          a4 == 8,
          p1.contains(1),
          p2.contains(2),
          p3.contains(3),
          p4.contains(4),
          p5.contains(5),
          p6.contains(6),
          p7.contains(7),
          p8.contains(8),
          p9.isEmpty,
        )
      },
      test("wraps + grows (backwards)") {
        val buffer: RingBuffer[Int] = new RingBuffer[Int](4)

        buffer.prepend(1)
        buffer.prepend(2)
        buffer.prepend(3)
        buffer.prepend(4)
        val u1 = buffer.used
        val a1 = buffer.allocated

        val p1 = buffer.popTail()
        val p2 = buffer.popTail()
        val u2 = buffer.used
        val a2 = buffer.allocated

        buffer.prepend(5)
        buffer.prepend(6)
        buffer.prepend(7)
        buffer.prepend(8)
        val u3 = buffer.used
        val a3 = buffer.allocated

        val p3 = buffer.popTail()
        val p4 = buffer.popTail()
        val p5 = buffer.popTail()
        val p6 = buffer.popTail()
        val p7 = buffer.popTail()
        val p8 = buffer.popTail()
        val p9 = buffer.popTail()
        val u4 = buffer.used
        val a4 = buffer.allocated

        assertTrue(
          u1 == 4,
          a1 == 4,
          u2 == 2,
          a2 == 4,
          u3 == 6,
          a3 == 8,
          u4 == 0,
          a4 == 8,
          p1.contains(1),
          p2.contains(2),
          p3.contains(3),
          p4.contains(4),
          p5.contains(5),
          p6.contains(6),
          p7.contains(7),
          p8.contains(8),
          p9.isEmpty,
        )
      },
    )

}
