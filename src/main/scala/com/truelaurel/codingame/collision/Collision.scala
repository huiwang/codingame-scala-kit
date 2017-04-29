package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl

case class Disk(p: Vectorl, v: Vectorl, r: Double, m: Double = 1.0) {
}

object DiskMover {
  def move(disk: Disk, time: Double): Disk = {
    disk.copy(p = disk.p + (disk.v * time))
  }
}

object Collision {
  def collideTime(d1: Disk, d2: Disk): Option[Double] = {
    collideTime(d1.p, d1.v, d1.r, d2.p, d2.v, d2.r)
  }

  def collideTime(p1: Vectorl, v1: Vectorl, r1: Double,
                  p2: Vectorl, v2: Vectorl, r2: Double): Option[Double] = {
    val dr = p2 - p1
    val dv = v2 - v1
    val dvdr = dv.dotProduct(dr)
    if (dvdr > 0) return None
    val dvdv = dv.dotProduct(dv)
    if (dvdv == 0) return None
    val drdr = dr.dotProduct(dr)
    val sigma = r1 + r2
    val d = (dvdr * dvdr) - dvdv * (drdr - sigma * sigma)
    if (d < 0) return None
    Some(-(dvdr + Math.sqrt(d)) / dvdv)
  }


  def bounceOff(p1: Vectorl, v1: Vectorl, m1: Double,
                p2: Vectorl, v2: Vectorl, m2: Double): (Vectorl, Vectorl) = {
    val impulse = computeImpulse(p1, v1, m1, p2, v2, m2)

    bouncedSpeed(v1, m1, v2, m2, impulse)
  }

  private def bouncedSpeed(v1: Vectorl, m1: Double, v2: Vectorl, m2: Double, impulse: Vectorl) = {
    (v1 + impulse / m1, v2 - impulse / m2)
  }

  def bounceOffWithMinimumImpulse(p1: Vectorl, v1: Vectorl, m1: Double,
                                  p2: Vectorl, v2: Vectorl, m2: Double,
                                  minImpulse: Double): (Vectorl, Vectorl) = {
    val impulse = computeImpulse(p1, v1, m1, p2, v2, m2)
    val adjusted = impulse.norm * (impulse.mag * 0.5 + minImpulse).max(impulse.mag)
    bouncedSpeed(v1, m1, v2, m2, adjusted)
  }


  private def computeImpulse(p1: Vectorl, v1: Vectorl, m1: Double, p2: Vectorl, v2: Vectorl, m2: Double) = {
    val dr = p2 - p1
    val dv = v2 - v1
    val drdr = dr.dotProduct(dr)
    val dvdr = dr.dotProduct(dv)
    val massCoefficient = (m1 + m2) / (m1 * m2)

    val impulse = dr * 2.0 * dvdr / (massCoefficient * drdr)
    impulse
  }

  def bounceOff(d1: Disk, d2: Disk): (Disk, Disk) = {
    val (v1, v2) = bounceOff(d1.p, d1.v, d1.m, d2.p, d2.v, d2.m)
    (d1.copy(v = v1), d2.copy(v = v2))
  }

  def move(disk: Disk, time: Double): Disk = DiskMover.move(disk, time)

}