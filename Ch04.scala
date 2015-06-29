object MainCode {

  sealed trait Option[+A] {
    // apply f if the Option is not None
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(x) => Some(f(x))
    }
    
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] = 
      map(f) getOrElse None

    def orElse[B >: A](ob: => Option[B]): Option[B] = 
      this map(Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = 
      flatMap(if (f(_)) Some(a) else None)

  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    a flatmap (aa => b map (bb => f(aa, bb)))

  def map2b[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = 
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E,B] = this match {
      case Right(a) => Right(f(a))
      case Left(b) => Left(b)
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Right(a) => f(a)
        case Left(b) => Left(e)
      }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = 
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }

    def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] = 
      for {
        a <- this
        bb <- b
      } yield f(a, bb)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = 
    traverse(es)(x => x)

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = 
    es.foldRight[Either[E,List[B]]](Right(Nil))((a,b) => f(a).map2(b)(_ :: _))



}
