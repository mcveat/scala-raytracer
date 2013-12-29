package srt.domain

import srt.Configuration._

/**
 * User: mcveat
 */
case class Color(red: Double, green: Double, blue: Double) {
  def + = op(_ + _)
  def *(d: Double) = op(_ * d)
  def /(d: Double) = op(_ / d)
  def toInt = new java.awt.Color((red * 255).toInt, (green * 255).toInt, (blue * 255).toInt).getRGB

  private def op(f: (Double, Double) => Double) = { (v: Color) =>
    val fn = f.tupled andThen norm
    Color(fn(red, v.red), fn(green, v.green), fn(blue, v.blue))
  }
  private def op(f: Double => Double) = {
    val fn = f andThen norm
    Color(fn(red), fn(green), fn(blue))
  }
  def norm(d: Double) = if (d < 0) 0d else if (d > 1) 1d else d

  def ambient = this * AMBIENT_COEFFICIENT
  def diffusedShade(factor: Double) = ambient + (if (factor > 0) this * factor * DIFFUSE_COEFFICIENT else Color.black)
}

object Color {
  val black = Color(0, 0, 0)
  val white = Color(1, 1, 1)
  val red = Color(1, 0, 0)
  val green = Color(0, 1, 0)
  val blue = Color(0, 0, 1)
  val dimGray = Color(0.41, 0.41, 0.41)

  def apply(v: Int): Color = {
    val c = new java.awt.Color(v)
    Color(c.getRed / 255d, c.getGreen / 255d, c.getBlue / 255d)
  }
}
