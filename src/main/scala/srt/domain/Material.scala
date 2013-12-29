package srt.domain

/**
 * User: mcveat
 */
trait Material {
  val reflectiveness: Double
  val specularReflectiveness: Double
  def colorAt(intersection: Intersection): Color
}

case class Solid(color: Color, override val reflectiveness: Double, override val specularReflectiveness: Double)
    extends Material {
  def colorAt(intersection: Intersection) = color
}

case class Textured(texture: Texture, uScale: Double, vScale: Double, override val reflectiveness: Double,
                    override val specularReflectiveness: Double) extends Material {
  lazy val width = texture.data.head.size
  lazy val height = texture.data.size
  def colorAt(intersection: Intersection) =  {
    val (u, v) = intersection.shape.textureCoordinatesAt(intersection.point)
    val du = (u / uScale).toInt % width
    val dv = (v / vScale).toInt % height
    val ddu = if (du < 0) width + du else du
    val ddv = if (dv < 0) height + dv else dv
    texture.data(ddu)(ddv)
  }
}
