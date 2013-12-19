package srt.domain

import srt.Configuration._

/**
 * User: mcveat
 */
trait Shape {
  val color: Color
  def ambientColor = color * AMBIENT_COEFFICIENT

  def intersectionWith(ray: Ray): Option[Intersection]
  def diffusedShadeColor(point: Vector, light: Light): Color
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
    if (discriminant <= 0) None else {
      val distance = -1 * (b + Math.sqrt(discriminant)) / 2 / a
      val point = ray.direction * distance + ray.position
      Some(Intersection(this, point, distance))
    }
  }
  def diffusedShadeColor(point: Vector, light: Light) = {
    val sphereDirection = ((point - position) / radius).normalize
    val lightDirection = (light.position - point).normalize
    val factor = sphereDirection dot lightDirection
    if (factor <= 0) ambientColor else color * factor * DIFFUSE_COEFFICIENT + ambientColor
  }
}
