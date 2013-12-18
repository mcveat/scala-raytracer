package srt

import Configuration._
import srt.domain.{Color, Ray, Scene, Vector}

/**
 * User: mcveat
 */
case class Tracer(scene: Scene) {
  def paint = for (x <- 0 until WIDTH) yield for (y <- (HEIGHT - 1).to(0, -1)) yield trace(x, y)
  def trace(x: Int, y: Int) = {
    val ray = Ray(Vector(x, y, 0), Vector(0, 0, 1))
    val intersectionColors = scene.shapes.map(s => (s.color, s.intersectionWith(ray)))
    val (color, distance) = intersectionColors.sortBy(_._2).head
    if (distance < Double.PositiveInfinity) color else Color.black
  }
}
