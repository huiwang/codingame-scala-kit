package com.truelaurel.algorithm.metaheuristic.evolutionstrategy

import com.truelaurel.algorithm.metaheuristic.model.Solution

class RockThrowingSolution extends Solution {
  var degree: Double = scala.util.Random.nextDouble * 90

  def setDegree(newdegree: Double): Unit = degree = Math.max(0.0, Math.min(90.0, newdegree))

  override def quality: Double = {
    val gravity = 1.0
    val initialSpeed = 1.0
    val verticalSpeed = initialSpeed * Math.sin(degree / 180 * Math.PI)
    val horizontalSpeed = initialSpeed * Math.cos(degree / 180 * Math.PI)
    val flightDuration = verticalSpeed / gravity * 2
    val distance = flightDuration * horizontalSpeed
    distance
  }
}
