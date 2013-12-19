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
    val intersections = shapes.map(intersect(ray))
    val possibleIntersection = intersections.flatten.sortBy(_.distance).headOption
    possibleIntersection.map(colorAtIntersection).getOrElse(Color.black)
  }

  def colorAtIntersection(intersection: Intersection) = {
    val Intersection(shape, point, _) = intersection
    val shadowRay = Ray(point, light.position - point)
    val intersections = shapes.filterNot(shape ==).map(intersect(shadowRay)).flatten
    val inShade = intersections.map(_.distanceTo(light.position)).exists(intersection.distanceTo(light.position) >)
    if (inShade) shape.ambientColor else shape.diffusedShadeColor(point, light)
  }

  def intersect(ray: Ray)(shape: Shape) = shape.intersectionWith(ray)
}
