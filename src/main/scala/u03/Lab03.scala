package u03

import Lists.*
import u02.Modules.Person
import u02.Modules.Person.Teacher
import u02.Optionals.*

import scala.annotation.tailrec

object Lab03:

  import List.*
  import Option.*

  // Task 1a, svolto da solo
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (_, 0) => l
    case (Nil(), _) => Nil()
    case (Cons(_, t), _) => drop(t, n - 1)

  // Task 1b, svolto da solo
  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Nil(), _) => right
    case (Cons(h1, t1), _) => Cons(h1, append(t1, right))

  // Task 1c, svolto da solo
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match
    case Nil() => Nil()
    case Cons(h, t) => append(f(h), flatMap(t)(f))

  // Task 2, svolto da solo
  def max(l: List[Int]): Option[Int] =
    def _max(i: Int, l: List[Int]): Int = (i, l) match
      case (_, Nil()) => i
      case (_, Cons(h, t)) if i >= _max(h, t) => i
      case (_, Cons(h, t)) if i < _max(h, t) => _max(h, t)
    l match
      case Nil() => None()
      case Cons(h, l) => Some(_max(h, l))

  // task 3, svolto da solo
  def getCourses(l: List[Person]): List[String] = l match
    case _ => flatMap(l)(f => f match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil())

  // Task 4, svolto da solo
  @tailrec
  def foldLeft[A, B](l: List[A])(v: B)(a: (B, A) => B): B = l match
    case Nil() => v
    case Cons(h, t) => foldLeft(t)(a(v, h))(a)

  def foldRight[A, B](l: List[A])(v: B)(a: (A, B) => B): B = l match
    case Nil() => v
    case Cons(h, t) => a(h, foldRight(t)(v)(a))


