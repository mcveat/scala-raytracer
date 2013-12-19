package srt.domain

import srt.Configuration._

/**
 * User: mcveat
 */
case class Scene(camera: Camera, shapes: List[Shape], light: Light) {
  def render = for (x <- 0 until WIDTH) yield for (y <- (HEIGHT - 1).to(0, -1)) yield trace(x, y)
  def trace(x: Int, y: Int) = {
    val planePosition = Vector(x, y, 0)
    val ray = Ray(planePosition, planePosition - camera.position)
    val intersectionColors = shapes.map(s => (s, s.intersectionWith(ray)))
    val (shape, distance) = intersectionColors.sortBy(_._2).head
    if (distance.isInfinite) Color.black
    else shape.diffusedShadeColor(distance, light, ray)
  }
}
