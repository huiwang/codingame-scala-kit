package com.truelaurel.codingame.collision

import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 02/12/2016.
  */
case class Wall(id: Int, from: Vectorl, to: Vectorl) extends Collidable {
  require(from.x == to.x || from.y == to.y, "must be horizontal or vertical wall")

  def distTo(p: Vectorl): Double = {
    if (isXSame) Math.abs(p.x - from.x) else Math.abs(p.y - from.y)
  }

  def isXSame: Boolean = from.x == to.x

  def inRange(p: Vectorl): Boolean =
    if (isXSame) (p.y - from.y) * (p.y - to.y) <= 0 else (p.x - from.x) * (p.x - to.x) <= 0

}