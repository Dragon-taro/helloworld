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
  val nil = part1.Nil
  val newList: part1.MyList[Int] = part1.Cons(100, list)

  println(part1.MyList.drop(newList, 2))
  println(part1.MyList.dropWhile(newList, (i: Int) => i == 1))
}
