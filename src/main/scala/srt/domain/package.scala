package srt

import java.io.File

/**
 * User: mcveat
 */
package object domain {
  case class Ray(position: Vector, direction: Vector)
  case class ViewPlane(width: Double, height: Double)
  case class Camera(position: Vector, direction: Vector, up: Vector, plane: ViewPlane, planeDistance: Double) {
    lazy val right = direction x up * (-1)

    def rayThrough(x: Double, y: Double) = {
      val upBy = up * (y - plane.height / 2)
      val rightBy = right * (x - plane.width / 2)
      val planePosition = position + (direction * planeDistance) + upBy + rightBy
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
  case class Texture(data: Seq[Seq[Color]])
  object Texture {
    def load(path: String) = Texture(Image.read(new File(path)))
  }
}
