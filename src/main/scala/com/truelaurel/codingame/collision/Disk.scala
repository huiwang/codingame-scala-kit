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
    val dr = d2.p - d1.p
    val dv = d2.v - d1.v
    val dvdr = dv.dotProduct(dr)
    if (dvdr > 0) return None
    val dvdv = dv.dotProduct(dv)
    if (dvdv == 0) return None
    val drdr = dr.dotProduct(dr)
    val sigma = d1.r + d2.r
    val d = (dvdr * dvdr) - dvdv * (drdr - sigma * sigma)
    if (d < 0) return None
    Some(-(dvdr + Math.sqrt(d)) / dvdv)
  }

  override def bounceOff(d1: Disk, d2: Disk): (Disk, Disk) = {
    val dr = d2.p - d1.p
    val dist = d1.r + d2.r
    val dv = d2.v - d1.v
    val dvdr = dr.dotProduct(dv)

    val force = 2 * d1.m * d2.m * dvdr / ((d1.m + d2.m) * dist)
    val fx = force * (d2.p.x - d1.p.x) / dist
    val fy = force * (d2.p.y - d1.p.y) / dist

    (d1.copy(v = d1.v + Vectorl(fx / d1.m, fy / d1.m)),
      d2.copy(v = d2.v - Vectorl(fx / d2.m, fy / d2.m)))
  }

  override def move(disk: Disk, time: Double): Disk = DiskMover.move(disk, time)

}