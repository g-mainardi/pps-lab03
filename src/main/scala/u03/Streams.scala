package u03

import u03.Sequences.Sequence

object Streams extends App:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3
    
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => Empty()

    def fill[A](n: Int)(el: A): Stream[A] = n match
      case n if n > 0 => cons(el, fill(n - 1)(el))
      case _ => Empty() 

    def fibonacci(): Stream[Int] =
      def _fib(n0: Int)(n1: Int): Stream[Int] = cons(n0 + n1, _fib(n1)(n0+n1))
      cons(0, cons(1, _fib(0)(1)))

    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = (s1, s2) match
      case (Cons(h, t), s) => cons(h(), interleave(s, t()))
      case (_, Cons(h, t)) => interleave(cons(h(), t()), Empty())
      case _ => Empty()

    def cycle[A](lst: Sequence[A]): Stream[A] =
      lazy val lista = lst
      def _cycle(lst: () => Sequence[A]): Stream[A] = lst() match
        case Sequence.Cons(h, t) => cons(h, _cycle(() => t))
        case _ => _cycle(lst)
      _cycle(() => lista)

    def cycle2[A](lst: => Sequence[A]): Stream[A] =
      lazy val lista = lst
      lista match
        case Sequence.Cons(h, t) => cons(h, cycle2(t))
        case _ => cycle2(lista)

    def toStream[A](lst: Sequence[A]): Stream[A] = lst match
      case Sequence.Cons(h, t) => cons(h, toStream(t))
      case _ => Empty()

    def cycleStream[A](lst: Sequence[A]): Stream[A] =
      lazy val str: Stream[A] = toStream(lst)
      def _cycle(s: => Stream[A]): Stream[A] = s match
      case Cons(h, t) => cons(h(), _cycle(t()))
      case _ => _cycle(s)
      _cycle(str)

    // Works with two values, to be extended for a list of values 
    def limitedCycle[A](first: => A)(second: => A): Stream[A] =
      cons(first, limitedCycle(second)(first))

  end Stream
end Streams

@main def tryStreams =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]
