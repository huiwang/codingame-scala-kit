package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by hwang on 01/12/2016.
  */
class CollisionSimulationTest extends FlatSpec with Matchers {
  val simulation = new CollisionSimulation[Disk](DiskCollider, 1.0)

  behavior of "A collision simulation"

  it should "return the moved entities when no collision is possible" in {
    val e0 = Disk(Vectorl(3, 3), Vectorl(-1, 0), 1)
    val e1 = Disk(Vectorl(7, 3), Vectorl(1, 0), 1)
    simulation.simulate(Vector(e0, e1)) should equal(List(DiskCollider.move(e0, 1),
      DiskCollider.move(e1, 1)))
  }

  it should "return the bounced entities when collision takes place" in {
    val d1 = Disk(Vectorl(3, 3), Vectorl(10, 0), 1)
    val d2 = Disk(Vectorl(7, 3), Vectorl(-10, 0), 1)
    val result = simulation.simulate(Vector(d1, d2))
    val expected = Vector(Disk(Vectorl(-5, 3), Vectorl(-10, 0), 1), Disk(Vectorl(15, 3), Vectorl(10, 0), 1))
    result should equal(expected)
  }

  it should "handle multiple collisions" in {
    val d0 = Disk(Vectorl(-2, 3), Vectorl(1, 0), 1)
    val d1 = Disk(Vectorl(7, 3), Vectorl(-1, 0), 1)
    val d2 = Disk(Vectorl(5, 7), Vectorl(0, -1), 1)

    val d0Moved = Disk(Vectorl(2, 3), Vectorl(1, 0), 1)
    val d1Moved = Disk(Vectorl(3, 1), Vectorl(-1, -1), 1)
    val d2Moved = Disk(Vectorl(5, 5), Vectorl(0, 0), 1)

    val result = new CollisionSimulation(DiskCollider, 4.0).simulate(Vector(d0, d1, d2))

    result should equal(List(d0Moved, d1Moved, d2Moved))

  }

  it should "handle different types of entities collision for example wall/disk" in {
    val diskWallSimul = new CollisionSimulation(DiskWallCollider)

    val topLeft = Vectorl(0, 0)
    val topRight = Vectorl(800, 0)
    val botLeft = Vectorl(0, 450)
    val botRight = Vectorl(800, 450)

    val topWall = Wall(-1, topLeft, topRight)
    val botWall = Wall(-2, botLeft, botRight)
    val leftWall = Wall(-3, topLeft, botLeft)
    val rightWall = Wall(-4, topRight, botRight)

    val walls: List[Collidable] = topWall :: botWall :: leftWall :: rightWall :: Nil
    val disk = Disk(Vectorl(10, 10), Vectorl(1, 1), 10, 1)

    val result = diskWallSimul.simulate(disk +: walls.toVector)

    result should equal(List(DiskCollider.move(disk, 1.0)) ::: walls)

  }

}
