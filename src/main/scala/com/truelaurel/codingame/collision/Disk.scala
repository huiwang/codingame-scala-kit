package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl

case class Disk(p: Vectorl, v: Vectorl, r: Double, m: Double = 1.0) extends Collidable {
}

object DiskMover {
  def move(disk: Disk, time: Double): Disk = {
    disk.copy(p = disk.p + (disk.v * time))
  }
}

object DiskCollider extends Collider[Disk] {
  override def collideTime(d1: Disk, d2: Disk): Option[Double] = {
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
    val dr = p2 - p1
    val dv = v2 - v1
    val drdr = dr.dotProduct(dr)
    val dvdr = dr.dotProduct(dv)
    val massCoefficient = (m1 + m2) / (m1 * m2)

    val impulse = dr * 2.0 * dvdr / (massCoefficient * drdr)
    (v1 + impulse / m1, v2 - impulse / m2)
  }


  override def bounceOff(d1: Disk, d2: Disk): (Disk, Disk) = {
    val (v1, v2) = bounceOff(d1.p, d1.v, d1.m, d2.p, d2.v, d2.m)
    (d1.copy(v = v1), d2.copy(v = v2))
  }

  override def move(disk: Disk, time: Double): Disk = DiskMover.move(disk, time)

}