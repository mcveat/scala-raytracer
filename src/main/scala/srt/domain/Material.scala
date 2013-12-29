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

    val uScaled = u * uScale * width
    val vScaled = v * vScale * height
    val uNorm = uScaled.toInt % width
    val vNorm = vScaled.toInt % height
    
    // texel
    val tu1 = if (uNorm < 0) width + uNorm else uNorm
    val tv1 = if (vNorm < 0) height + vNorm else vNorm
    val tu2 = (tu1 + 1) % width
    val tv2 = (tv1 + 1) % height
    
    val uFractional = uScaled - Math.floor(uScaled)
    val vFractional = vScaled - Math.floor(vScaled)
    
    // weight factors
    val w1 = (1 - uFractional) * (1 - vFractional)
    val w2 = uFractional * (1 - vFractional)
    val w3 = (1 - uFractional) * vFractional
    val w4 = uFractional *  vFractional

    // colors
    val c1 = texture.data(tv1)(tu1)
    val c2 = texture.data(tv1)(tu2)
    val c3 = texture.data(tv2)(tu1)
    val c4 = texture.data(tv2)(tu2)

    // bilinear filtered color
    (c1 * w1) + (c2 * w2) + (c3 * w3) + (c4 * w4)
  }
}
