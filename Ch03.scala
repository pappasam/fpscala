/*
 * This is a test script
 * for chapter 3; not intended for compilation
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

// Test value for exercises
val test = List(1,2,3,4)
println(test)

// Exercise 3.2
def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
}
println(tail(test))

// Exercise 3.3
def setHead[A](h: A, l: List[A]): List[A] = l match {
  case Nil => Cons(h, Nil)
  case Cons(x, xs) => Cons(h, xs)
}
println(setHead(2, test))

// Exercise 3.4
def drop[A](l: List[A], n: Int): List[A] = 
  if (l == Nil) Nil
  else if (n <= 0) l
  else drop(tail(l), n-1)
println(drop(test, 2))

// Exercise 3.5
def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
  case Cons(x,xs) if f(x) => dropWhile(xs)(f)
  case _ => l
}
println(dropWhile(test)(_ < 2))

// Exercise 3.6
