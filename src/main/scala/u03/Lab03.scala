package u03

import Lists.*
import u02.Modules.Person
import u02.Modules.Person.Teacher
import u02.Optionals.*
import u03.Streams.*

import scala.annotation.tailrec

object Lab03 extends App:

  import List.*
  import Option.*
  import Stream.*

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

  // Task 1d, svolto da solo
  def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
    case Cons(_, _) => flatMap(l)(h => Cons(mapper(h), Nil()))
    case Nil() => Nil()

  def filter[A](l: List[A])(pred: A => Boolean): List[A] = l match
    case Cons(h, _)  => flatMap(l)(f => f match
      case _ if pred(f) => Cons(f, Nil())
      case _ => Nil())
    case _ => Nil()

  // Task 2, svolto da solo
  def max(l: List[Int]): Option[Int] =
    def _max(i: Int, l: List[Int]): Int = (i, l) match
      case (_, Cons(h, t)) if i >= _max(h, t) => i
      case (_, Cons(h, t)) if i < _max(h, t) => _max(h, t)
      case _ => i
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

  // Task 6, svolto da solo
  def constant[A](v: A): Stream[A] =
    Stream.cons(v, constant(v))

  // Task 7, svolto da solo
  def iterate[A](i1: => A)(i2: => A)(next: (A, A) => A): Stream[A] =
    cons(i1, iterate(i2)(next(i1, i2))(next))

  val fibs: Stream[Int] = iterate(0)(1)(_ + _)