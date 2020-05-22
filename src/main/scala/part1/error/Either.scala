package part1.error

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(l)  => Left(l)
    case Right(r) => Right(f(r))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(v) => f(v)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(r) => Right(r)
  }

  def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      a <- this
      bb <- b
    } yield f(a, bb)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil    => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  def sequense[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty list")
    else Right(xs.sum / xs.length)

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def parseInsuranceRateQuote(
    age: String,
    numberOfSpeedingTickets: String
  ): Either[Exception, Double] =
    for {
      a <- Try { age.toInt }
      tickets <- Try { numberOfSpeedingTickets.toInt }
    } yield a * tickets

//  def sequense[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
//    case Left(e) => Left

}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("name is required")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("age is out of range")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def mkPersonViaFor(name: String, age: Int): Either[String, Person] =
    for {
      n <- mkName(name)
      a <- mkAge(age)
    } yield Person(n, a)
}
