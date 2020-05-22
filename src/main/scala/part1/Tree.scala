package part1

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Branch(l, r) => 1 + size(l) + size(r)
      case Leaf(_)      => 1
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(_ + _ + 1)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v)      => v
  }

  def maxViaFold(t: Tree[Int]): Int = fold(t)(n => n)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(_ => 0)(1 + _ max _)

//  def depth[A](t: Tree[A]): Int = {
//    def loop(t: Tree[A], n: Int): Int = {
//      t match {
//        case Branch(l, r) => loop(l, n + 1) max loop(r, n + 1)
//        case Leaf(_)      => n
//      }
//    }
//
//    loop(t, 0)
//  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Branch(l, r) => depth(l) max depth(r) + 1
      case Leaf(_)      => 0
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v)      => Leaf(f(v))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      case Leaf(v)      => f(v)
    }
  }

}
