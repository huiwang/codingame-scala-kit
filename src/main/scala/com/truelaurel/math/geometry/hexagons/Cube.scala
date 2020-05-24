package com.truelaurel.math.geometry.hexagons

/**
  * Created by hwang on 15/04/2017.
  */
case class Cube(x: Int, y: Int, z: Int) {

  def toOffset: Offset = {
    val newX = x + (z - (z & 1)) / 2
    val newY = z
    Offset(newX, newY)
  }

  def neighbor(orientation: Int): Cube = {
    Cube(
      x + Cube.directions(orientation)(0),
      y + Cube.directions(orientation)(1),
      z + Cube.directions(orientation)(2)
    )
  }

  def distanceTo(that: Cube): Int =
    (Math.abs(x - that.x) + Math.abs(y - that.y) + Math.abs(z - that.z)) / 2

}

object Cube {
  val directions: Array[Array[Int]] = Array[Array[Int]](
    Array(1, -1, 0),
    Array(+1, 0, -1),
    Array(0, +1, -1),
    Array(-1, +1, 0),
    Array(-1, 0, +1),
    Array(0, -1, +1)
  )

  def apply(x: Int, y: Int): Cube = {
    Offset(x, y).toCube
  }
}
