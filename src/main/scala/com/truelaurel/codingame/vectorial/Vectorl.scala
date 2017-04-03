package com.truelaurel.codingame.vectorial

import com.truelaurel.codingame.math.Mathl

case class Vectorl(x: Double, y: Double) {
  lazy val mag2: Double = x * x + y * y
  lazy val mag: Double = Math.sqrt(mag2)
  lazy val norm: Vectorl = if (mag > 0) this * (1.0 / mag) else Vectorl(0, 0)

  def dotProduct(that: Vectorl): Double = x * that.x + y * that.y

  def *(factor: Double) = Vectorl(x * factor, y * factor)

  def +(that: Vectorl) = Vectorl(x + that.x, y + that.y)

  def -(that: Vectorl) = Vectorl(x - that.x, y - that.y)

  def perp = Vectorl(-y, x)

  def perDotProduct(that : Vectorl) : Double = perp.dotProduct(that)

  def between(v1 : Vectorl, v2 : Vectorl) : Boolean = {
    this.perDotProduct(v1) * this.perDotProduct(v2) < 0
  }

  override def equals(o: Any): Boolean = o match {
    case that: Vectorl => Mathl.almostEqual(x, that.x) && Mathl.almostEqual(y, that.y)
    case _ => false
  }

}
