package srt

import srt.Configuration._

/**
 * User: mcveat
 */
package object domain {
  case class Ray(position: Vector, direction: Vector)
  case class Camera(position: Vector, direction: Vector, up: Vector, planeDistance: Double) {
    def rayThrough(x: Double, y: Double) = {
      val right = direction crossProduct up * (-1)
      val planePosition = position + (direction * planeDistance) + (up * (y - HEIGHT / 2)) + (right * (x - WIDTH / 2))
      val rayDirection = planePosition - position
      Ray(position, rayDirection.normalize)
    }
  }
  case class Light(position: Vector, color: Color)
  case class Intersection(ray: Ray, shape: Shape, point: Vector, distance: Double) {
    def distanceTo(p: Vector) = (p - point).length
    def reflectedRay = {
      val normal = shape.normalVectorAt(point)
      val direction = normal * -2 * (ray.direction dot normal) + ray.direction
      Ray(point, direction)
    }
  }
  case class Material(color: Color, reflectiveness: Double, specularReflectiveness: Double) {
    def ambientColor = color * AMBIENT_COEFFICIENT
    def diffusedShadeColor(factor: Double) =
      if (factor <= 0) ambientColor else color * factor * DIFFUSE_COEFFICIENT + ambientColor
  }
}
