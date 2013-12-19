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
      val a = ray.direction dot ray.direction
      val b = (ray.position - position) * 2 dot ray.direction
      val c = (position dot position) +
              (ray.position dot ray.position) -
              (ray.position dot position * 2) -
              Math.pow(radius, 2)
      val discriminant = Math.pow(b, 2) - (4 * a * c)
      if (c < 0) Double.PositiveInfinity else -1 * (b + Math.sqrt(discriminant)) / 2 / a
    }
  }

  case class Camera(position: Vector)

  case class Scene(camera: Camera, shapes: List[Shape])
}
