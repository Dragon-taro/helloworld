package part1

object Mod {
  def fib(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n == 0 || n == 1) n + acc
      else go(n - 1, acc) + go(n - 2, acc)
    }

    go(n, 0)
  }

  def isSorted[A](
    as: Array[A]
  )(implicit ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (ordered(as(n), as(n + 1))) loop(n + 1)
      else false
    }

    loop(0)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = { a => b =>
    f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) =>
    f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = { a =>
    f(g(a))
  }
}

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = {
    ints match {
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)
    }
  }

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Nil           => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](list: MyList[A], head: A): MyList[A] = list match {
    case Nil           => Nil
    case Cons(_, tail) => Cons(head, tail)
  }

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil           => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }

  }

  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = {
    l match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _                  => l
    }
  }

  def init[A](l: MyList[A]): MyList[A] = {
    l match {
      case Nil              => Nil
      case Cons(_, Nil)     => l
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }

  def append[A](l1: MyList[A], l2: MyList[A]): MyList[A] = {
    l1 match {
      case Nil              => l2
      case Cons(head, tail) => Cons(head, append(tail, l2))
    }
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def foldRightViaFoldLeft[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def appendViaFoldLeft[A](l1: MyList[A], l2: MyList[A]): MyList[A] = {
    foldLeft(reverse(l1), l2)((b, a) => Cons(a, b))
  }

  def reverse[A](l: MyList[A]): MyList[A] = {
    foldLeft(l, MyList[A]())((a, b) => Cons(b, a))
  }

  def sum2(l: MyList[Int]): Int = {
    foldRight(l, 0)(_ + _)
  }

  def length[A](as: MyList[A]): Int = {
    foldRight(as, 0)((_, i) => i + 1)
  }

  def add(l: MyList[Int]): MyList[Int] = {
    foldRightViaFoldLeft(l, MyList[Int]())((a, b) => Cons(a + 1, b))
  }

  def add1(l: MyList[Int]): MyList[Int] = {
    map(l)(_ + 1)
  }

  def map[A, B](l: MyList[A])(f: A => B): MyList[B] = {
    foldRightViaFoldLeft(l, MyList[B]())((a, b) => Cons(f(a), b))
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    foldRightViaFoldLeft(as, MyList[A]())((a, b) => {
      if (f(a)) Cons(a, b)
      else b
    })
  }

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
    foldRightViaFoldLeft(as, MyList[B]())((a, b) => append(f(a), b))
  }

  def addPairwise(l1: MyList[Int], l2: MyList[Int]): MyList[Int] = {
    (l1, l2) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
    }
  }

  def zipWith[A, B, C](l1: MyList[A],
                       l2: MyList[B])(f: (A, B) => C): MyList[C] = {
    (l1, l2) match {
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }
}
