package task2

import org.junit.*
import org.junit.Assert.*
import task2.PersonFunctions.getCourses

class PersonFunctionsTest:
  import u02.Modules.*
  import Person.*
  import u03.Sequences.*
  import Sequence.*

  private val course1 = "PPS"
  private val course2 = "LCMC"
  private val course3 = "PCD"
  private val teacher1: Person = Teacher("Viroli", course1)
  private val teacher2: Person = Teacher("Bravetti", course2)
  private val teacher3: Person = Teacher("Ricci", course3)
  private val student1: Person = Student("Gio", 2000)
  private val student2: Person = Student("Momo", 2001)
  private val student3: Person = Student("Ale", 2002)
  val teachers: Sequence[Person] = Cons(teacher1, Cons(teacher2, Cons(teacher3, Nil())))
  val students: Sequence[Person] = Cons(student1, Cons(student2, Cons(student3, Nil())))
  val people: Sequence[Person] = concat(teachers, students)
  val courses: Sequence[String] = Cons(course1, Cons(course2, Cons(course3, Nil())))

  @Test def testGetCourses() =
    assertEquals(courses, getCourses(teachers))
    assertEquals(courses, getCourses(people))

end PersonFunctionsTest
