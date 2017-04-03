package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 02/12/2016.
  */
class DiskWallCollisionResolverTest extends FlatSpec with Matchers {

  val disk = Disk(0, Vectorl(0, 0), Vectorl(1, 1), 1)
  behavior of "A disk wall collision resolver for collision time prediction"

  it should "return none if wall is too short" in {
    val bxWOutOfRange = Wall(1, Vectorl(10, 0), Vectorl(10, -5))
    val byWOutOfRange = Wall(1, Vectorl(-10, 10), Vectorl(0, 10))
    DiskWallCollisionResolver.collideTime(disk, bxWOutOfRange) should equal(None)
    DiskWallCollisionResolver.collideTime(bxWOutOfRange, disk) should equal(None)
    DiskWallCollisionResolver.collideTime(disk, byWOutOfRange) should equal(None)
    DiskWallCollisionResolver.collideTime(byWOutOfRange, disk) should equal(None)
  }

  it should "return some time if wall is in long enough" in {
    val bxWInRange = Wall(1, Vectorl(10, 0), Vectorl(10, 100))
    val byWInRange = Wall(1, Vectorl(0, 10), Vectorl(100, 10))
    DiskWallCollisionResolver.collideTime(disk, bxWInRange) should equal(Some(9.0))
    DiskWallCollisionResolver.collideTime(bxWInRange, disk) should equal(Some(9.0))
    DiskWallCollisionResolver.collideTime(disk, byWInRange) should equal(Some(9.0))
    DiskWallCollisionResolver.collideTime(byWInRange, disk) should equal(Some(9.0))
  }

  it should "return none if disk is going in the wrong direction" in {
    val topWall = Wall(1, Vectorl(0, 0), Vectorl(800, 0))
    val movingBotRight = Disk(0, Vectorl(10, 10), Vectorl(1, 1), 10.0)
    DiskWallCollisionResolver.collideTime(topWall, movingBotRight) should equal(None)
  }

  behavior of "A disk wall collision resolver for bounce off"

  it should "bounce off the disk" in {
    val disk = Disk(0, Vectorl(9, 9), Vectorl(1, 1), 1)
    val bxWInRange = Wall(1, Vectorl(10, 0), Vectorl(10, 100))
    val byWInRange = Wall(1, Vectorl(0, 10), Vectorl(100, 10))
    DiskWallCollisionResolver.bounceOff(disk, bxWInRange) should equal((Disk(0, Vectorl(9, 9), Vectorl(-1, 1), 1), bxWInRange))
    DiskWallCollisionResolver.bounceOff(disk, byWInRange) should equal((Disk(0, Vectorl(9, 9), Vectorl(1, -1), 1), byWInRange))
  }
}
