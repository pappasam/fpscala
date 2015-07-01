trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL * 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = 
  rng => (a, rng)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeEven: Rand[Int] = 
  map(nonNegativeInt)(i => i - i % 2)

// EXERCISES BELOW THIS POINT
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (n,r) = rng.nextInt
  (if (n < 0) -(n + 1) else n, r)
}

def double(rng:RNG): (Double, RNG) = {
  val (n,r) = nonNegativeInt(rng)
  (n / (Int.MaxValue.toDouble + 1.0), r)
}

def intDouble(rng: RNG): ((Int, Double), RNG) = {
  val (ni, ri) = nonNegativeInt(rng)
  val (nd, rd) = double(ri)
  ((ni, nd), rd)
}

def doubleInt(rng: RNG): ((Double, Int), RNG) = {
  val ((ni, nd), r) = intDouble(rng)
  ((nd, ni), r)
}

def double3(rng: RNG): ((Double, Double, Double), RNG) = {
  val (n1, r1) = double(rng)
  val (n2, r2) = double(r1)
  val (n3, r3) = double(r2)
  ((n1, n2, n3), r3)
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) = 
  if (count == 0)
    (List(), rng)
  else {
    val (x, r1) = rng.nextInt
    val (xs, r2) = ints(count - 1)(r1)
    (x :: xs, r2)
  }

def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
  def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) = 
    if (count == 0)
      (xs, r)
    else {
      val (x, r2) = r.nextInt
      go(count - 1, r2, x :: xs)
    }
  go(count, rng, List())
}

val _double: Rand[Double] = 
  map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1.0))

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
  rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a,b), rng3)
  }

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

def _ints(count: Int): Rand[List[Int]] = 
  sequence(List.fill(count)(int))


