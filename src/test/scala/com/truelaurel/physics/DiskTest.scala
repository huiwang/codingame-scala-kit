package com.truelaurel.physics

import com.truelaurel.math.geometry.Vectorl
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 01/12/2016.
  */
class DiskTest extends FlatSpec with Matchers {
  val disk: Disk = Disk(p = Vectorl(2, 1), v = Vectorl(2, 3), r = 1)

  val diskCollisionResolver = Collision

  behavior of "A disk"

  it can "move" in {
    diskCollisionResolver.move(disk, 1.0) should equal(
      Disk(Vectorl(4, 4), Vectorl(2, 3), 1)
    )
  }

  behavior of "A disk for collision detection"

  it should "return none when it moves away from another disk" in {
    diskCollisionResolver.collideTime(
      disk,
      Disk(Vectorl(-2, -5), Vectorl(-2, -3), 1)
    ) should equal(None)
  }

  it should "return none when it's relatively still than another disk" in {
    diskCollisionResolver.collideTime(
      disk,
      Disk(Vectorl(10, 10), Vectorl(2, 3), 2)
    ) should equal(None)
  }

  it should "return none when another disk is moving too fast" in {
    diskCollisionResolver.collideTime(
      disk,
      Disk(Vectorl(10, 10), Vectorl(-100, 0), 2)
    ) should equal(None)
  }

  it should "return collision time when collision is going to happen" in {
    diskCollisionResolver.collideTime(
      disk,
      Disk(Vectorl(5, 1), Vectorl(-2, 3), 1)
    ) should equal(Some(0.25))
  }

  behavior of "A disk for bounce off"

  it should "return disks going to their opposite way in case of face-to-face collision" in {
    val goingRight: Disk = Disk(Vectorl(3, 3), Vectorl(1, 0), 1)
    val goingLeft: Disk = Disk(Vectorl(5, 3), Vectorl(-1, 0), 1)
    val (d1, d2) = diskCollisionResolver.bounceOff(goingRight, goingLeft)
    d1 should equal(Disk(Vectorl(3, 3), Vectorl(-1, 0), 1))
    d2 should equal(Disk(Vectorl(5, 3), Vectorl(1, 0), 1))
  }

  it should "return disks going to opposite way with different speed in case of heavy-light face-to-face collision" in {
    val goingRight: Disk = Disk(Vectorl(3, 3), Vectorl(1, 0), 1, 2)
    val goingLeft: Disk = Disk(Vectorl(5, 3), Vectorl(-1, 0), 1, 1)

    val (d1, d2) = diskCollisionResolver.bounceOff(goingRight, goingLeft)
    d1 should equal(Disk(Vectorl(3, 3), Vectorl(-1 / 3.0, 0), 1, 2))
    d2 should equal(Disk(Vectorl(5, 3), Vectorl(5 / 3.0, 0), 1, 1))
  }

}
