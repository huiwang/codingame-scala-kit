package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 02/12/2016.
  */
object DiskWallCollisionResolver extends CollisionResolver[Collidable] {
  override def collideTime(c1: Collidable, c2: Collidable): Option[Double] = (c1, c2) match {
    case (d1: Disk, d2: Disk) => DiskCollisionResolver.collideTime(d1, d2)
    case (d: Disk, w: Wall) => wallDiskCollisionTime(d, w)
    case (w: Wall, d: Disk) => wallDiskCollisionTime(d, w)
    case _ => None
  }

  override def move(collidable: Collidable, time: Double): Collidable = collidable match {
    case d: Disk => DiskCollisionResolver.move(d, time)
    case a => a
  }

  private def wallDiskCollisionTime(d: Disk, w: Wall) = {
    if (towardWall(d, w)) {
      val time = w.distTo(d.position) - d.radius
      val moved = DiskCollisionResolver.move(d, time)
      if (w.inRange(moved.position)) Some(time) else None
    } else {
      None
    }
  }

  private def towardWall(disk: Disk, wall: Wall) = {
    if (wall.isXSame) {
      disk.speed.x * (disk.position.x - wall.from.x) < 0
    } else {
      disk.speed.y * (disk.position.y - wall.from.y) < 0
    }
  }

  override def bounceOff(c1: Collidable, c2: Collidable): (Collidable, Collidable) = (c1, c2) match {
    case (d1: Disk, d2: Disk) => DiskCollisionResolver.bounceOff(d1, d2)
    case (d: Disk, w: Wall) => (bounce(w, d), w)
    case (w: Wall, d: Disk) => (w, bounce(w, d))
    case _ => (c1, c2)
  }

  def bounce(w: Wall, d: Disk): Disk =
    if (w.isXSame)
      Disk(d.id, d.position, Vectorl(-d.speed.x, d.speed.y), d.radius, d.mass)
    else
      Disk(d.id, d.position, Vectorl(d.speed.x, -d.speed.y), d.radius, d.mass)
}
