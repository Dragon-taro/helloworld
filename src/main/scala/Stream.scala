trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _          => acc
    }

    go(this, List()).reverse
  }

  def taken(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case Cons(h, t) if n >= 1 => Stream.cons(h(), t().taken(n - 1))
    case _                    => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty      => Empty
    case Cons(_, t) => t().drop(n - 1)
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
    case _                    => Empty
  }

  def takeWhileViaFoldRight(f: A => Boolean): Stream[A] =
    foldRight(Stream[A]())(
      (a, b) => if (f(a)) Stream.cons(a, b) else Stream.empty
    )

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])(
      (h, t) =>
        if (f(h)) Stream.cons(h, t)
        else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def constant2[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a, a))

  def from(n: Int): Stream[Int] = {
    lazy val tail = Cons(() => n, () => from(n + 1))
    tail
  }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some((s, s + 1)))

  val fibs = {
    def go(f1: Int, f2: Int): Stream[Int] =
      cons(f1, go(f1, f1 + f2))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None         => empty
    }

}
