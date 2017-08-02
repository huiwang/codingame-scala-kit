package com.truelaurel.algorithm.metaheuristic.genetic

case class Referee(wps: Array[WayPoint] = null) {
  val wpsize = wps.size

  def evaluate(ms: MoveSet): Double = {
    val pod = new Pod
    var i = 1
    for (v <- ms.vecarr) {
      pod.move(v)
      if (wps(i % wpsize).touch(pod)) {
        i += 1
      }
    }
    i * 1000 - wps(i % wpsize).dist(pod)
  }
}
