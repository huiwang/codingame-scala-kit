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

  def collisionTime(ship: Ship, cube: Cube): Int = {
    val distance = ship.center.distanceTo(cube)
    val angle = CaribbeanContext.angle(ship.center, cube)
    val diff = (ship.orientation - angle).abs
    val realDiff = diff.min(6 - diff)
    if(realDiff == 0) {
      distance - 1
    } else {
      stop(ship) + realDiff + distance - 1
    }
  }

  private def stop(ship: Ship): Int = ship.speed match {
    case 0 => 0
    case 1 => 1
    case 2 => 3
    case _ => throw new IllegalArgumentException("unknown speed")
  }

}
