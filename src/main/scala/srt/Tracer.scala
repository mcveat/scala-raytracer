package srt

import Configuration._
import srt.domain.{Color, Ray, Scene, Vector}

/**
 * User: mcveat
 */
case class Tracer(scene: Scene) {
  def paint = for (x <- 0 until WIDTH) yield for (y <- (HEIGHT - 1).to(0, -1)) yield trace(x, y)
  def trace(x: Int, y: Int) = {
    val planePosition = Vector(x, y, 0)
    val ray = Ray(planePosition, planePosition - scene.camera.position)
    val intersectionColors = scene.shapes.map(s => (s, s.intersectionWith(ray)))
    val (shape, distance) = intersectionColors.sortBy(_._2).head
    if (distance.isInfinite) Color.black
    else shape.diffusedShadeColor(distance, scene.light, ray)
  }
}
