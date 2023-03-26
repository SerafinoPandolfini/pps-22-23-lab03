package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.Optionals.*
import Lab03.*
import u02.Modules.*
import u03.Streams.*

class Lab03Test:

  import List.*
  import Option.*
  import Person.*
  import Streams.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  @Test def testAppend() =
    val tail = Cons(40, Nil())
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Lab03.flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      Lab03.flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), Lab03.map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), Lab03.map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), Lab03.filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), Lab03.filter(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(None(), max(Nil()))

  @Test def testGetCourses() =
    val persons = Cons(Student("Mario", 2015), Cons(Teacher("Luigi", "Test"), Nil()))
    assertEquals(Cons("Test", Nil()), getCourses(persons))
    val persons2 = Cons(Teacher("Luigi", "Test"), Cons(Student("Mario", 2015), Nil()))
    assertEquals(Cons("Test", Nil()), getCourses(persons2))
    val empty: List[Person] = Nil()
    assertEquals(Nil(), getCourses(empty))
    val students = Cons(Student("Luigi", 2014), Cons(Student("Mario", 2015), Nil()))
    assertEquals(Nil(), getCourses(students))
    val teachers = Cons(Teacher("Luigi", "test"), Cons(Teacher("Mario", "prova"), Nil()))
    assertEquals(Cons("test", Cons("prova", Nil())), getCourses(teachers))

  @Test def testFold() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(-8, foldRight(lst)(0)(_ - _))

  @Test def testStreamDrop() =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Cons(6, Cons(7, Cons(8, Cons(9, Nil())))), Stream.toList(Stream.drop(s)(6)))

  @Test def testConstant() =
    assertEquals(Cons ("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))),
      Stream.toList(Stream.take(constant("x"))(5)))

  @Test def testFibs() =
    assertEquals(Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Cons(5, Cons(8, Cons(13, Nil())))))))),
      Stream.toList(Stream.take(fibs)(8)))
