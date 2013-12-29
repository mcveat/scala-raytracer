package srt.domain

import srt.Configuration._

/**
  * User: mcveat
 */
trait Shape {
  val material: Material

  def getColorAt(intersection: Intersection, light: Light) =
    List(diffusedShadeColor _, specularReflection _).map(_.apply(intersection, light)).reduceLeft(_ + _)

  def specularReflection(intersection: Intersection, light: Light): Color = {
    val lightDirection = (light.position - intersection.point).normalize
    val surfaceNormal = normalVectorAt(intersection.point)
    val reflectedLight = lightDirection - (surfaceNormal * (lightDirection dot surfaceNormal) * 2)
    val specularCoefficient = intersection.ray.direction dot reflectedLight
    if (specularCoefficient <= 0) return Color.black
    val specularFactor = Math.pow(specularCoefficient, SPECULAR_COEFFICIENT_POWER) * material.specularReflectiveness
    light.color * specularFactor
  }

  def diffusedShadeColor(intersection: Intersection, light: Light): Color = {
    val lightDirection = (light.position - intersection.point).normalize
    val factor = normalVectorAt(intersection.point) dot lightDirection
    material.colorAt(intersection).diffusedShade(factor)
  }

  def intersectionWith(ray: Ray): Option[Intersection]
  def normalVectorAt(point: Vector): Vector
  def textureCoordinatesAt(point: Vector): (Double, Double)
}

case class Sphere(position: Vector, radius: Double, override val material: Material) extends Shape {
  lazy val north = Vector(0, 1, 0).normalize
  lazy val east = Vector(1, 0, 0).normalize
  lazy val equator = north x east

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

  def normalVectorAt(point: Vector) = (point - position) / radius
  def textureCoordinatesAt(point: Vector): (Double, Double) = {
    val pointDirection = (point - position).normalize
    val latitude = Math.acos((north dot pointDirection) * -1)
    val v = latitude / Math.PI
    val longitude = Math.acos(east dot pointDirection / Math.sin(latitude)) / 2 / Math.PI
    val u = if ((equator dot pointDirection) >= 0) 1 - longitude else longitude
    (u, v)
  }
}

case class Plane(normal: Vector, distance: Double, override val material: Material) extends Shape {
  val uAxis = Vector(normal.y, normal.z, normal.x * -1)
  val vAxis = uAxis x normal

  def intersectionWith(ray: Ray): Option[Intersection] = {
    val d = normal dot ray.direction
    if (d == 0) return None
    val intersectionDistance = -(normal dot ray.position + distance) / d
    if (intersectionDistance < 0) return None
    val point = ray.position + ray.direction * intersectionDistance
    Some(Intersection(ray, this, point, intersectionDistance))
  }
  def normalVectorAt(point: Vector) = normal
  def textureCoordinatesAt(point: Vector) = (point dot uAxis, point dot vAxis)
}
