package allTasks

import u03.Sequences.*
import Sequence.*

import scala.annotation.tailrec

class allExercises:
  object Task1:
    @annotation.tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(_, t) if n > 0 => skip(t)(n - 1)
      case _ => s

    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Cons(h, t), s) => Cons(h, concat(t, s))
      case (_, s) => s

    def reverse[A](s: Sequence[A]): Sequence[A] =
      @tailrec
      def _reverse(s: Sequence[A], acc: Sequence[A]): Sequence[A] = (s, acc) match
        case (Cons(h1, t1), acc) => _reverse(t1, Cons(h1, acc))
        case _ => acc
      _reverse(s, Nil())

    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()
  end Task1

  object Task1Optional:
    import u03.Optionals.*
    import Optional.*

    def min(s: Sequence[Int]): Optional[Int] =
      def _min(s: Sequence[Int])(minValue: Optional[Int]): Optional[Int] = s match
        case Cons(h, t) if h < orElse(minValue, Int.MaxValue) => _min(t)(Just(h))
        case Cons(_, t) => _min(t)(minValue)
        case _ => minValue
      _min(s)(Empty())

    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      def _even(s: Sequence[A])(even: Boolean): Sequence[A] = (s, even) match
        case (Cons(h, t), true) => Cons(h, _even(t)(false))
        case (Cons(_, t), false) => _even(t)(true)
        case _ => Nil()
      _even(s)(true)

    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) => elem == h | contains(t)(elem)
      case _ => false

    def distinct[A](s: Sequence[A]): Sequence[A] =
      def _distinct(s: Sequence[A])(acc: Sequence[A]): Sequence[A] = s match
        case Cons(h, t) if !contains(acc)(h) => Cons(h, _distinct(t)(Cons(h, acc)))
        case Cons(_, t) => _distinct(t)(acc)
        case _ => Nil()
      _distinct(s)(Nil())

    def group[A](s: Sequence[A]): Sequence[Sequence[A]] =
      def _group(s: Sequence[A])(acc: Sequence[A]): Sequence[Sequence[A]] = (s, acc) match
        case (Cons(h1, t), Cons(h2, _)) if h1 != h2 => Cons(acc, _group(t)(Cons(h1, Nil())))
        case (Cons(h, t), _) => _group(t)(Cons(h, acc))
        case (_, Nil()) => Nil()
        case _ => Cons(acc, Nil())
      _group(s)(Nil())

    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      (filter(s)(pred), filter(s)(a => !pred(a)))

    def partitionTailReversed[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      @tailrec
      def _partition(s: Sequence[A])(acc1: Sequence[A])(acc2: Sequence[A]): (Sequence[A], Sequence[A]) = s match
        case Cons(h, t) if pred(h) => _partition(t)(Cons(h, acc1))(acc2)
        case Cons(h, t) => _partition(t)(acc1)(Cons(h, acc2))
        case _ => (acc1, acc2)
      _partition(s)(Nil())(Nil())
  end Task1Optional

  object Task2:
    import u02.Modules.*
    import Person.*
    
    val isTeacher: Person => Boolean =
      case Teacher(_, _) => true
      case _ => false
    val getCourse: Person => String =
      case Teacher(_, c) => c
      case _ => ""

    def getCourses(s: Sequence[Person]): Sequence[String] =
      map(filter(s)(isTeacher))(getCourse)

    @tailrec
    def foldLeft[A, B](s: Sequence[A])(acc: B)(operator: (B, A) => B): B = s match
      case Cons(h, t) => foldLeft(t)(operator(acc, h))(operator)
      case _ => acc

    def countCourses(s: Sequence[Person]): Int =
      foldLeft(map(filter(s)(isTeacher))(_ => 1))(0)(_ + _)
  end Task2

  // Task 3
  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:
    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    // Task 3 mandatory points
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(h, t) if pred(h()) => cons(h(), takeWhile(t())(pred))
      case _ => empty()

    def fill[A](n: Int)(el: A): Stream[A] = n match
      case n if n > 0 => cons(el, fill(n - 1)(el))
      case _ => empty()

    def fibonacci(): Stream[Int] =
      def _fib(n0: Int)(n1: Int): Stream[Int] =
        cons(n0, _fib(n1)(n0 + n1))
      _fib(0)(1)

    // Task 3 optional points
    def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = s1 match
      case Cons(h, t) => cons(h(), interleave(s2, t()))
      case _ => s2

    // Attempts for point 10: cycle infinite list
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
  