package srt.domain

import srt.Configuration._
import srt.Progress

/**
 * User: mcveat
 */
case class Scene(camera: Camera, shapes: List[Shape], lights: List[Light]) {
  def render(progress: Progress) =
    for (x <- 0 until WIDTH) yield for (y <- (HEIGHT - 1).to(0, -1)) yield progress.advanceAfter(trace(x, y))
  def trace(x: Int, y: Int): Color = trace(camera.rayThrough(x, y), shapes, TRACING_DEPTH)

  private def trace(ray: Ray, shapes: List[Shape], depth: Int): Color = {
    val intersections = shapes.map(intersect(ray))
    val possibleIntersection = intersections.flatten.sortBy(_.distance).headOption
    possibleIntersection.map(colorAtIntersection(_, depth)).getOrElse(Color.black)
  }

  private def colorAtIntersection(intersection: Intersection, depth: Int): Color = {
    val shape = intersection.shape
    val otherShapes = shapes.filterNot(shape ==)
    val baseColor = lights.map(colorFromLight(intersection, _)).map(_ / lights.size).reduceLeft(_ + _)
    if (depth == 0 || shape.material.reflectiveness == 0d) return baseColor
    val reflection = trace(intersection.reflectedRay, otherShapes, depth - 1) * shape.material.reflectiveness
    reflection + (baseColor * (1 - shape.material.reflectiveness))
  }

  private def colorFromLight(intersection: Intersection, light: Light) = {
    val Intersection(_, shape, point, _) = intersection
    val shadowRay = Ray(point, light.position - point)
    val shadeIntersections = shapes.filterNot(intersection.shape ==).map(intersect(shadowRay)).flatten
    val inShade = shadeIntersections.map(_.distanceTo(light.position)).exists(intersection.distanceTo(light.position) >)
    if (inShade) shape.material.colorAt(intersection).ambient else shape.getColorAt(intersection, light)
  }

  private def intersect(ray: Ray)(shape: Shape) = shape.intersectionWith(ray)
}
