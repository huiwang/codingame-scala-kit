package com.truelaurel.algorithm.metaheuristic.genetic

class WayPoint(val x: Int, val y: Int) {
  var next: WayPoint = null

  def dist(p: Pod): Double =
    Math.sqrt(Math.pow(x - p.x, 2) + Math.pow(y - p.y, 2))

  def touch(p: Pod): Boolean = dist(p) < 400
}
