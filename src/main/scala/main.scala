import part1.error.Person
import part1.{Branch, Leaf, Tree}

import scala.util.{Failure, Success}

object HelloWorld extends App {
  val fib = part1.Mod.fib(6)
  println(fib)

  val sortedArr = Array[Int](1, 2, 3, 4)
  val nonSortedArr = Array[Int](1, 2, 4, 3)

  implicit def sortFunc(num1: Int, num2: Int): Boolean = num1 < num2

  println(part1.Mod.isSorted(sortedArr))
  println(part1.Mod.isSorted(nonSortedArr))

  val list = part1.MyList(1, 2, 3)
  val list2 = part1.MyList(10, 20, 30, 40)
  val nil = part1.Nil
  val newList: part1.MyList[Int] = part1.Cons(100, list)

  println(part1.MyList.addPairwise(list, list2))
  println(part1.MyList.filter(list)(_ > 1))

  val t: Tree[Int] = Branch(
    Branch(Leaf(1), Leaf(2)),
    Branch(Leaf(3), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(8))))
  )
  println(Tree.depth(t))
  println(Tree.map(t)(_ * 10))

  val p = Person.mkPersonViaFor("", 10)
  println(p)

  val s = Stream(1, 2, 3).taken(2)
  println(s)

  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.taken(2).toList)

  val const1 = Stream.constant(1)
  val const2 = Stream.constant2(1)
  val nums = Stream.from(10).taken(10).toList

  println(const1)
  println(const2)
  println(nums)
}
