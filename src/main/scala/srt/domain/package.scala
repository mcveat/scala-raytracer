package srt

/**
 * User: mcveat
 */
package object domain {
  case class Ray(position: Vector, direction: Vector)
  case class Camera(position: Vector)
  case class Light(position: Vector)
  case class Intersection(shape: Shape, point: Vector, distance: Double) {
    def distanceTo(p: Vector) = (p - point).length
  }
}
