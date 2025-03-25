package task2

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

import scala.annotation.tailrec

object MoreOnLists:
  val isTeacher: Person => Boolean =
    case Teacher(_, _) => true
    case _ => false
  val courseOrElseEmpty: Person => String =
    case Teacher(_, c) => c
    case _ => ""
  def getCourses(s: Sequence[Person]): Sequence[String] =
      map(filter(s)(isTeacher))(courseOrElseEmpty)

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(acc: B)(operator: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(operator(acc,h))(operator)
    case _ => acc

  def countCourses(s: Sequence[Person]): Int =
    foldLeft(map(filter(s)(isTeacher))(_ => 1))(0)(_ + _)