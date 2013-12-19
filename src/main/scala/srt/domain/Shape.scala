package srt.domain

import srt.Configuration._

/**
 * User: mcveat
 */
trait Shape {
  val color: Color
  def intersectionWith(ray: Ray): Double
  def diffusedShadeColor(distance: Double, light: Light, viewPointRay: Ray): Color
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
    if (discriminant <= 0) Double.PositiveInfinity else -1 * (b + Math.sqrt(discriminant)) / 2 / a
  }
  def diffusedShadeColor(distance: Double, light: Light, viewPointRay: Ray) = {
    val point = viewPointRay.direction * distance + viewPointRay.position
    val sphereDirection = ((point - position) / radius).normalize
    val lightDirection = (light.position - point).normalize
    val factor = sphereDirection dot lightDirection
    val ambient = color * AMBIENT_COEFFICIENT
    if (factor <= 0) ambient else color * factor * DIFFUSE_COEFFICIENT + ambient
  }
}
