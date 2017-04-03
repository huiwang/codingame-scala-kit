package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl

case class Disk(id: Int,
                position: Vectorl,
                speed: Vectorl,
                radius: Double,
                mass: Double = 1.0) extends Collidable {
}

object DiskCollisionResolver extends CollisionResolver[Disk] {
  override def collideTime(d1: Disk, d2: Disk): Option[Double] = {
    val dr = d2.position - d1.position
    val dv = d2.speed - d1.speed
    val dvdr = dv.dotProduct(dr)
    if (dvdr > 0) return None
    val dvdv = dv.dotProduct(dv)
    if (dvdv == 0) return None
    val drdr = dr.dotProduct(dr)
    val sigma = d1.radius + d2.radius
    val d = (dvdr * dvdr) - dvdv * (drdr - sigma * sigma)
    if (d < 0) return None
    Some(-(dvdr + Math.sqrt(d)) / dvdv)

  }

  override def bounceOff(d1: Disk, d2: Disk): (Disk, Disk) = {
    val dr = d2.position - d1.position
    val dist = d1.radius + d2.radius
    val dv = d2.speed - d1.speed
    val dvdr = dr.dotProduct(dv)

    val force = 2 * d1.mass * d2.mass * dvdr / ((d1.mass + d2.mass) * dist)
    val fx = force * (d2.position.x - d1.position.x) / dist
    val fy = force * (d2.position.y - d1.position.y) / dist

    (d1.copy(speed = d1.speed + Vectorl(fx / d1.mass, fy / d1.mass)),
      d2.copy(speed = d2.speed - Vectorl(fx / d2.mass, fy / d2.mass)))
  }

  override def move(disk: Disk, time: Double): Disk = {
    disk.copy(position = disk.position + (disk.speed * time))
  }

}