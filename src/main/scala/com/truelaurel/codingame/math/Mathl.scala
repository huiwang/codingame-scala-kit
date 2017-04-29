package com.truelaurel.codingame.math

import scala.util.Random

/**
  * Created by hwang on 01/12/2016.
  */
object Mathl {

  val random = new Random(62638886242411L)

  def halfUp(d: Double): Int = ((d.abs * 2 + 1) / 2).toInt * (if (d > 0) 1 else -1)

  def almostEqual(d1: Double, d2: Double): Boolean = Math.abs(d1 - d2) <= 0.000001

  def randomBetween(min: Double, max: Double): Double = {
    min + random.nextDouble() * (max - min)
  }
}
