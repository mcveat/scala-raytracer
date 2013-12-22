package srt.domain

/**
  * User: mcveat
 */
trait Shape {
  val material: Material

  def intersectionWith(ray: Ray): Option[Intersection]
  def diffusedShadeColor(point: Vector, light: Light): Color
  def normalVectorAt(point: Vector): Vector
}

case class Sphere(position: Vector, radius: Double, override val material: Material) extends Shape {
  def intersectionWith(ray: Ray): Option[Intersection] = {
    val a = ray.direction dot ray.direction
    val b = (ray.position - position) * 2 dot ray.direction
    val c = (position dot position) +
      (ray.position dot ray.position) -
      (ray.position dot position * 2) -
      Math.pow(radius, 2)
      val discriminant = Math.pow(b, 2) - (4 * a * c)
    if (discriminant <= 0) return None
    val distance = (b + Math.sqrt(discriminant)) / -2 / a
    val point = ray.direction * distance + ray.position
    if (((point - ray.position) dot ray.direction) > 0d) Some(Intersection(ray, this, point, distance)) else None
  }
  def diffusedShadeColor(point: Vector, light: Light) = {
    val sphereDirection = ((point - position) / radius).normalize
    val lightDirection = (light.position - point).normalize
    val factor = sphereDirection dot lightDirection
    material.diffusedShadeColor(factor)
  }
  def normalVectorAt(point: Vector) = (point - position) / radius
}
