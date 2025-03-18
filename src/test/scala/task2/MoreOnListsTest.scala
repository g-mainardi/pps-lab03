package task2

import org.junit.*
import org.junit.Assert.*
import task2.MoreOnLists.*

class MoreOnListsTest:
  import u02.Modules.*
  import Person.*
  import u03.Sequences.*
  import Sequence.*

  val course1 = "PPS"
  val course2 = "LCMC"
  val course3 = "PCD"
  val teacher1: Person = Teacher("Viroli", course1)
  val teacher2: Person = Teacher("Bravetti", course2)
  val teacher3: Person = Teacher("Ricci", course3)
  val student1: Person = Student("Gio", 2000)
  val student2: Person = Student("Momo", 2001)
  val student3: Person = Student("Ale", 2002)
  val teachers: Sequence[Person] = Cons(teacher1, Cons(teacher2, Cons(teacher3, Nil())))
  val students: Sequence[Person] = Cons(student1, Cons(student2, Cons(student3, Nil())))
  val people: Sequence[Person] = concat(teachers, students)
  val courses: Sequence[String] = Cons(course1, Cons(course2, Cons(course3, Nil())))

  @Test def testGetCourses() =
    assertEquals(courses, getCourses(teachers))
    assertEquals(courses, getCourses(people))

end PersonFunctionsTest
end MoreOnListsTest
