package task2

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

object PersonFunctions:
  def getCourses(s: Sequence[Person]): Sequence[String] =
    val isTeacher: Person => Boolean =
      case Teacher(_, _) => true
      case _ => false
    val getCourse: Person => String =
      case Teacher(_, c) => c
      case _ => ""
    map(filter(s)(isTeacher))(getCourse)
