package com.truelaurel.math.geometry.hexagons

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

  def angle(target: Offset): Double = {
    val dy = (target.y - this.y) * Math.sqrt(3) / 2
    val dx = target.x - this.x + ((this.y - target.y) & 1) * 0.5
    var angle = -Math.atan2(dy, dx) * 3 / Math.PI
    if (angle < 0) angle += 6
    else if (angle >= 6) angle -= 6
    angle
  }

}






