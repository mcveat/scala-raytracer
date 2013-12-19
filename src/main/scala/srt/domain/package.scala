package srt

/**
 * User: mcveat
 */
package object domain {
  case class Ray(position: Vector, direction: Vector)
  case class Camera(position: Vector)
  case class Light(position: Vector)
  case class Scene(camera: Camera, shapes: List[Shape], light: Light)
}
