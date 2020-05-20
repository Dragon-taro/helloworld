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
    def loop(list: MyList[A], i: Int): MyList[A] = {
      list match {
        case Nil           => Nil
        case _ if i == n   => list
        case Cons(_, tail) => loop(tail, i + 1)
      }
    }

    loop(l, 0)
  }

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = {
    def loop(list: MyList[A], i: Int): MyList[A] = {
      list match {
        case Nil                      => Nil
        case Cons(head, _) if f(head) => list
        case Cons(_, tail)            => loop(tail, i + 1)
      }
    }

    loop(l, 0)
  }
}
