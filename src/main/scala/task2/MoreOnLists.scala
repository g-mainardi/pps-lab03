package task2

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

import scala.annotation.tailrec

object MoreOnLists:
  def getCourses(s: Sequence[Person]): Sequence[String] =
    val isTeacher: Person => Boolean =
      case Teacher(_, _) => true
      case _ => false
    val getCourse: Person => String =
      case Teacher(_, c) => c
      case _ => ""
    map(filter(s)(isTeacher))(getCourse)

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(acc: B)(operator: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(operator(acc,h))(operator)
    case _ => acc