package com.truelaurel.codingame.marslander

import scala.io.StdIn

/**
  * Created by hwang on 25/03/2017.
  */
object Player {

  def main(args: Array[String]): Unit = {
    // the number of points used to draw the surface of Mars.
    val surfacePoints = 0.until(StdIn.readInt()).map(_ => {
      val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
      (x, y)
    }).toVector
    surfacePoints

    while(true) {
      // hspeed: the horizontal speed (in m/s), can be negative.
      // vspeed: the vertical speed (in m/s), can be negative.
      // fuel: the quantity of remaining fuel in liters.
      // rotate: the rotation angle in degrees (-90 to 90).
      // power: the thrust power (0 to 4).
      val Array(x, y, hspeed, vspeed, fuel, rotate, power) = for(i <- readLine split " ") yield i.toInt

      // rotate power. rotate is the desired rotation angle. power is the desired thrust power.
      println("-20 3")
    }
  }

}
