abstract class Point {
  def length(): Double
}

class Point2D(_x: Int, _y: Int) extends Point with ToString {
  private val x = _x
  private val y = _y

  def length(): Double = {
    math.sqrt(x * x + y * y)
  }

  override def toString(): String = {
    s"x: $x, y: $y"
  }
}

trait ToString {
  def toString(): String
}

class Point3D(val x: Int, val y: Int, val z: Int) extends Point with ToString {
  def length(): Double = {
    math.sqrt(x * x + y * y + z * z)
  }

  override def toString() = {
    s"x: $x, y: $y, z: $z"
  }
}
