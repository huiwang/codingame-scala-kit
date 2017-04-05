package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 02/12/2016.
  */
object DiskWallCollider extends Collider[Collidable] {
  override def collideTime(c1: Collidable, c2: Collidable): Option[Double] = (c1, c2) match {
    case (d1: Disk, d2: Disk) => DiskCollider.collideTime(d1, d2)
    case (d: Disk, w: Wall) => wallDiskCollisionTime(d, w)
    case (w: Wall, d: Disk) => wallDiskCollisionTime(d, w)
    case _ => None
  }

  override def move(collidable: Collidable, time: Double): Collidable = collidable match {
    case d: Disk => DiskMover.move(d, time)
    case a => a
  }

  private def wallDiskCollisionTime(d: Disk, w: Wall) = {
    if (towardWall(d, w)) {
      val time = w.distTo(d.p) - d.r
      val moved = DiskMover.move(d, time)
      if (w.inRange(moved.p)) Some(time) else None
    } else {
      None
    }
  }

  private def towardWall(disk: Disk, wall: Wall) = {
    if (wall.isXSame) {
      disk.v.x * (disk.p.x - wall.from.x) < 0
    } else {
      disk.v.y * (disk.p.y - wall.from.y) < 0
    }
  }

  override def bounceOff(c1: Collidable, c2: Collidable): (Collidable, Collidable) = (c1, c2) match {
    case (d1: Disk, d2: Disk) => DiskCollider.bounceOff(d1, d2)
    case (d: Disk, w: Wall) => (bounce(w, d), w)
    case (w: Wall, d: Disk) => (w, bounce(w, d))
    case _ => (c1, c2)
  }

  def bounce(w: Wall, d: Disk): Disk =
    if (w.isXSame)
      Disk(d.p, Vectorl(-d.v.x, d.v.y), d.r, d.m)
    else
      Disk(d.p, Vectorl(d.v.x, -d.v.y), d.r, d.m)
}
