package u03
import org.junit.*
import org.junit.Assert.*

import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*

class StreamTest:

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    val str1 = Stream.toList(Stream.fill(3)("a"))
    assertEquals(Cons("a", Cons("a", Cons("a", Nil()))), str1)
  
  @Test def testFibonacci(): Unit =
    val fibonacci: Stream[Int] = Stream.fibonacci()
    val str1 = Stream.toList(Stream.take(fibonacci)(5))
    val str2 = Stream.toList(Stream.take(fibonacci)(8))
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil()))))), str1)
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))), str2)

  @Test def testInterleave(): Unit =
    val s1 = Stream.take(Stream.iterate(1)(_ + 2))(3)
    val s2 = Stream.take(Stream.iterate(2)(_ + 2))(5)
    val result = Stream.toList(Stream.interleave(s1, s2))
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(8, Cons(10, Nil())))))))), result)

  @Test def testLimitedCycle(): Unit =
    val repeatStrings = limitedCycle("a")("b")
    val repeatInt = limitedCycle(1)(2)
    assertEquals(Cons("a", Cons("b", Cons("a", Cons("b", Cons("a", Nil()))))), toList(Stream.take(repeatStrings)(5)))
    assertEquals(Cons(1, Cons(2, Cons(1, Cons(2, Nil())))), toList(Stream.take(repeatInt)(4)))

  @Test def testCycle(): Unit =
    val repeatStrings = cycle(Cons("a", Cons("b", Cons("c", Nil()))))
    val repeatInt = cycle(Cons(1, Cons(2, Nil())))
    assertEquals(Cons("a", Cons("b", Cons("c", Cons("a", Cons("b", Nil()))))), toList(Stream.take(repeatStrings)(5)))
    assertEquals(Cons(1, Cons(2, Cons(1, Cons(2, Nil())))), toList(Stream.take(repeatInt)(4)))

end StreamTest
