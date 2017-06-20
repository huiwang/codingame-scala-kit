package com.truelaurel.algorithm.metaheuristic.hillclimbing

import com.truelaurel.algorithm.metaheuristic.model.{Problem, Solution}
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.Duration

/**
  * Only better solution leads to new exploration
  */
class HillClimbing(duration: Duration) {

  val chrono = new Chronometer(duration)

  def search[S <: Solution](problem: Problem[S]): S = {
    var solution = problem.randomSolution()
    chrono.start()
    while (!chrono.willOutOfTime) {
      val tweaked = problem.tweakSolution(solution)
      solution = if (tweaked.quality() > solution.quality()) tweaked else solution
    }
    solution
  }

}
