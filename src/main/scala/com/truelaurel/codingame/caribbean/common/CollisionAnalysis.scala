package com.truelaurel.codingame.caribbean.common

import com.truelaurel.codingame.hexagons.{Cube, Offset}

/**
  * Created by hwang on 22/04/2017.
  */
object CollisionAnalysis {

  def hitMyself(ship: Ship): Cube = {
    ship.speed match {
      case 0 => ship.center
      case 1 => ship.nextBow
      case 2 => throw new IllegalStateException("unable to hit myself")
    }
  }

  def travelTime(distance: Int): Int = {
    (1 + (distance / 3.0).round).toInt
  }
}
