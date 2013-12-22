package srt

import srt.Configuration._

/**
 * User: mcveat
 */
package object domain {
  case class Ray(position: Vector, direction: Vector)
  case class Camera(position: Vector)
  case class Light(position: Vector)
  case class Intersection(ray: Ray, shape: Shape, point: Vector, distance: Double) {
    def distanceTo(p: Vector) = (p - point).length
    def reflectedRay = {
      val normal = shape.normalVectorAt(point)
      val direction = normal * -2 * (ray.direction dot normal) + ray.direction
      Ray(point, direction)
    }
  }
  case class Material(color: Color, reflectiveness: Double) {
    def ambientColor = color * AMBIENT_COEFFICIENT
    def diffusedShadeColor(factor: Double) =
      if (factor <= 0) ambientColor else color * factor * DIFFUSE_COEFFICIENT + ambientColor
  }
}
