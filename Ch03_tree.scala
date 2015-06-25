object TreeObject {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(n) => n
      case Branch(l,r) => maximum(l) max maximum(r)
    }

    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
      case Leaf(a) => f(a)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }

    def size2[A](t: Tree[A]): Int = 
      fold(t)(a => 1)(1 + _ + _)

    def maximum2(t: Tree[Int]): Int = 
      fold(t)(_)(_ max _)

    def depth2[A](t: Tree[A]): Int = 
      fold(t)(a => 0)(1 + (_ max _))

    def map2[A](t: Tree[A])(f: A => B): Tree[B] = t match {
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
    }
  }
}
