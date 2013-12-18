package srt

/**
 * User: mcveat
 */
package object domain {
  case class Color(red: Double, green: Double, blue: Double) {
    def toInt = ((red * 255).toInt << 16) | ((green * 255).toInt << 8) | (blue * 255).toInt
  }

  object Color {
    val black = Color(0, 0, 0)
    val white = Color(1, 1, 1)
    val red = Color(1, 0, 0)
    val green = Color(0, 1, 0)
    val blue = Color(0, 0, 1)
  }

  case class Vector(x: Double, y: Double, z: Double) {
    def + = op(_ + _)
    def - = op(_ - _)
    def * = op(_ * _)
    def *(d: Double) = op(_ * d)
    def / = op(_ / _)
    def /(d: Double) = op(_ / d)
    def dot(v: Vector) = (this * v).elementsSum

    private def op(f: (Double, Double) => Double) = (v: Vector) => Vector(f(x, v.x), f(y, v.y), f(z, v.z))
    private def op(f: Double => Double) = Vector(f(x), f(y), f(z))
    private def elementsSum = x + y + z
  }

  case class Ray(position: Vector, direction: Vector)

  trait Shape {
    val color: Color
    def intersectionWith(ray: Ray): Double
  }
  case class Sphere(position: Vector, radius: Double, override val color: Color) extends Shape {
    def intersectionWith(ray: Ray) = {
      val v = position - ray.position
      val a = v dot ray.direction
      val b = (v dot v) - (radius * radius)
      val c = a * a - b
      if (c >= 0) a - Math.sqrt(c) else Double.PositiveInfinity
    }
  }

  case class Scene(shapes: List[Shape])
}
