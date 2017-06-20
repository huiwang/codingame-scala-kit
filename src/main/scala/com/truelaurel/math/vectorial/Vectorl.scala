package com.truelaurel.math.vectorial

import com.truelaurel.math.Mathl

object Vectorls {
  val origin = Vectorl(0, 0)
  val axisX = Vectorl(1, 0)
  val axisY = Vectorl(0, 1)
}

case class Vectorl(x: Double, y: Double) {
  lazy val mag2: Double = x * x + y * y
  lazy val mag: Double = Math.sqrt(mag2)
  lazy val norm: Vectorl = if (mag > 0) this * (1.0 / mag) else Vectorl(0, 0)

  def /(factor : Double): Vectorl = this * (1.0 / factor)

  def +(that: Vectorl): Vectorl = Vectorl(x + that.x, y + that.y)

  def -(that: Vectorl): Vectorl = Vectorl(x - that.x, y - that.y)

  def pivotTo(desired: Vectorl, maxDegree: Double): Vectorl = {
    if (mag2 == 0 || angleInDegreeBetween(desired) <= maxDegree) {
      desired.norm
    } else {
      if (this.perDotProduct(desired) > 0) {
        rotateInDegree(maxDegree).norm
      } else {
        rotateInDegree(-maxDegree).norm
      }
    }
  }

  def rotateInDegree(degree: Double): Vectorl = rotateInRadian(Math.toRadians(degree))

  def rotateInRadian(radians: Double): Vectorl = {
    val rotated = angleInRadian + radians
    Vectorl(Math.cos(rotated), Math.sin(rotated)) * mag
  }

  def *(factor: Double): Vectorl = Vectorl(x * factor, y * factor)

  private def angleInRadian: Double = Math.atan2(y, x)

  def angleInDegreeBetween(other: Vectorl): Double = {
    Math.toDegrees(angleInRadianBetween(other))
  }

  def angleInRadianBetween(other: Vectorl): Double = {
    val result = this.dotProduct(other) / (this.mag * other.mag)
    if (result >= 1.0) 0 else Math.acos(result)
  }

  def perDotProduct(that: Vectorl): Double = perp.dotProduct(that)

  def dotProduct(that: Vectorl): Double = x * that.x + y * that.y

  def perp = Vectorl(-y, x)

  def between(v1: Vectorl, v2: Vectorl): Boolean = {
    this.perDotProduct(v1) * this.perDotProduct(v2) < 0
  }

  def truncate = Vectorl(x.toInt, y.toInt)

  def round = Vectorl(Mathl.halfUp(x), Mathl.halfUp(y))

  override def equals(o: Any): Boolean = o match {
    case that: Vectorl => Mathl.almostEqual(x, that.x) && Mathl.almostEqual(y, that.y)
    case _ => false
  }

  private def angleInDegree: Double = Math.toDegrees(angleInRadian)

}
