package com.truelaurel.codingame.hexagons

/**
  * Created by hwang on 15/04/2017.
  */
case class Offset(x : Int, y : Int) {
  def toCube: Cube = {
    val xp = x - (y - (y & 1)) / 2
    val zp = y
    val yp = -(xp + zp)
    Cube(xp, yp, zp)
  }

}






