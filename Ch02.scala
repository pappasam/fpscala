import annotation.tailrec

// Exercise 1
def fib(n: Int): Int = {
  @tailrec
  def go(acc: Int, n1: Int, n2: Int): Int = {
    if (acc <= 1)
      n1
    else
      go(acc-1, n2, n2+n1)
  }
  go(n, 0, 1)
}
Range(1,10).map(fib).foreach(println)

// Exercise 2
def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
  @tailrec
  def g(n: Int): Boolean = 
    if (n >= as.length - 1)
      true
    else if (gt(as(n), as(n+1)))
      false
    else
      go(n+1)
  go(0)
}

// Exercise 3
def curry[A,B,C](f: (A,B) => C): A => (B => C) = 
  a => b => f(a,b)

// Exercise 4
def uncurry[A,B,C](f: A => B => C): (A, B) => C = 
  (a,b) => f(a)(b)

// Exercise 5
def compose[A,B,C](f: B => C, g: A => B): A => C =
  a  => f(g(a))

