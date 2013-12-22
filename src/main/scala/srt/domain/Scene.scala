package srt.domain

import srt.Configuration._

/**
 * User: mcveat
 */
case class Scene(camera: Camera, shapes: List[Shape], light: Light) {
  def render = for (x <- 0 until WIDTH) yield for (y <- (HEIGHT - 1).to(0, -1)) yield trace(x, y)
  def trace(x: Int, y: Int): Color = trace(camera.rayThrough(x, y), shapes, TRACING_DEPTH)

  private def trace(ray: Ray, shapes: List[Shape], depth: Int): Color = {
    val intersections = shapes.map(intersect(ray))
    val possibleIntersection = intersections.flatten.sortBy(_.distance).headOption
    possibleIntersection.map(colorAtIntersection(_, depth)).getOrElse(Color.black)
  }

  private def colorAtIntersection(intersection: Intersection, depth: Int) = {
    val Intersection(_, shape, point, _) = intersection
    val shadowRay = Ray(point, light.position - point)
    val otherShapes = shapes.filterNot(shape ==)
    val intersections = otherShapes.map(intersect(shadowRay)).flatten
    val inShade = intersections.map(_.distanceTo(light.position)).exists(intersection.distanceTo(light.position) >)
    val calculatedColor = if (inShade) shape.material.ambientColor else shape.diffusedShadeColor(point, light)
    if (depth == 0 || shape.material.reflectiveness == 0d) calculatedColor
    else
      (trace(intersection.reflectedRay, otherShapes, depth - 1) * shape.material.reflectiveness) +
      (calculatedColor * (1 - shape.material.reflectiveness))
  }

  private def intersect(ray: Ray)(shape: Shape) = shape.intersectionWith(ray)
}
