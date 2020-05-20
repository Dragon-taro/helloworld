import scala.util.{Failure, Try}

object ErrorHandling {
  def div(num1: Int, num2: Int) = {
    if (num2 != 4) Try(num1 / num2)
    else Failure(new Error("4 is dead number!!"))
  }
}
