package srt.domain

/**
 * User: mcveat
 */
case class Color(red: Double, green: Double, blue: Double) {
  def + = op(_ + _)
  def *(d: Double) = op(d * _)
  def toInt = ((red * 255).toInt << 16) | ((green * 255).toInt << 8) | (blue * 255).toInt

  private def op(f: (Double, Double) => Double) = { (v: Color) =>
    val fn = f.tupled andThen norm
    Color(fn(red, v.red), fn(green, v.green), fn(blue, v.blue))
  }
  private def op(f: Double => Double) = {
    val fn = f andThen norm
    Color(fn(red), fn(green), fn(blue))
  }
  def norm(d: Double) = if (d < 0) 0d else if (d > 1) 1d else d
}

object Color {
  val black = Color(0, 0, 0)
  val white = Color(1, 1, 1)
  val red = Color(1, 0, 0)
  val green = Color(0, 1, 0)
  val blue = Color(0, 0, 1)
}
