import annotation.tailrec

/*
 * This is a test_list script
 * for chapter 3; not intended for compilation
 */
object DataStructures {
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

    def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = 
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    // EXERCISE SOLUTIONS

    // Exercise 3.2
    def tail[A](l: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(x,xs) => xs
    }

    // Exercise 3.3
    def setHead[A](h: A, l: List[A]): List[A] = l match {
      case Nil => Cons(h, Nil)
      case Cons(x, xs) => Cons(h, xs)
    }

    // Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] = 
      if (l == Nil) Nil
      else if (n <= 0) l
      else drop(tail(l), n-1)

    // Exercise 3.5
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(x,xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

    // Exercise 3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x,xs) => Cons(x, init(xs))
    }

    // Exercise 3.9
    def length[A](as: List[A]): Int = 
      foldRight(as, 0)((_, y) => 1 + y)

    // Exercise 3.10
    @tailrec
    def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = as match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }

    // Exercise 3.11
    def sum2(as: List[Int]): Int = 
      foldLeft(as, 0)(_ + _)

    def product2(as: List[Double]): Double = 
      foldLeft(as, 1.0)(_ * _)

    def length2[A](as: List[A]): Int = 
      foldLeft(as, 0)((x,_) => x + 1)

    // Exercise 3.12
    def reverse[A](as: List[A]): List[A] = 
      foldLeft(as, Nil: List[A])((x,y) => Cons(y,x))

    // Exercise 3.13
    def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B = 
      foldLeft(reverse(as), z)((x,y) => f(y,x)) 

    // Exercise 3.14
    def append[A](l: List[A], r: List[A]): List[A] = 
      foldRight(l,r)(Cons(_,_))

    // Exercise 3.15
    def flatten[A](l: List[List[A]]): List[A] = 
      foldRight(l, Nil: List[A])(append)

    // Exercise 3.16
    def add1(l: List[Int]): List[Int] = 
      foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))

    // Exercise 3.17
    def doubleToString(l: List[Double]): List[String] = 
      foldRight(l, Nil: List[String])((h,t) => Cons(h.toString, t))

    // Exercise 3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = 
      foldRight(l, Nil: List[B])((h,t) => Cons(f(a), t))

    // Exercise 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      foldRight(as, Nil: List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  }

  def main(args: Array[String]): Unit = {
    import List._
    val test_list = List(1,2,3,4,6,7,8,9,10)
    val test_list_double = List(1.0, 2.0, 3.0)

    println("Starter String: " + test_list)
    println("3.2: " + tail(test_list))
    println("3.3: " + setHead(2, test_list))
    println("3.4: " + drop(test_list, 2))
    println("3.5: " + dropWhile(test_list)(_ < 2))
    println("3.6: " + init(test_list))
    println("3.9: " + length(test_list).toString)
    println("3.10: " + foldLeft(test_list, 0)(_ + _))
    println("3.11 (sum): " + sum2(test_list).toString)
    println("3.11 (product): " + product2(test_list_double).toString)
    println("3.11 (length): " + length2(test_list).toString)
    println("3.12: " + reverse(test_list))
  }
}

