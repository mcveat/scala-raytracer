package srt.domain

/**
 * User: mcveat
 */
case class Vector(x: Double, y: Double, z: Double) {
  def + = op(_ + _)
  def - = op(_ - _)
  def * = op(_ * _)
  def *(d: Double) = op(_ * d)
  def / = op(_ / _)
  def /(d: Double) = op(_ / d)
  def dot(v: Vector) = (this * v).elementsSum
  def length = Math.sqrt(this dot this)
  def normalize = this / length
  def crossProduct(other: Vector) = Vector(
    y * other.z - z * other.y,
    z * other.x - x * other.z,
    x * other.y - y * other.x
  )

  private def op(f: (Double, Double) => Double) = (v: Vector) => Vector(f(x, v.x), f(y, v.y), f(z, v.z))
  private def op(f: Double => Double) = Vector(f(x), f(y), f(z))
  private def elementsSum = x + y + z
}
