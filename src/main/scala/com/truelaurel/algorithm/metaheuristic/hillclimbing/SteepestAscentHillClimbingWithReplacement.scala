package com.truelaurel.algorithm.metaheuristic.hillclimbing

import com.truelaurel.algorithm.metaheuristic.model.{Problem, Solution}
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.Duration

/**
  * By having a high selection pressure, we eliminate many poor solutions and stick with the best one.
  * Therefore, we can have more exploitation with a high pressure.
  */
class SteepestAscentHillClimbingWithReplacement(
    duration: Duration,
    selectionPressure: Int
) {

  val chrono = new Chronometer(duration)
  val selectionRange: Range = 0 until selectionPressure

  def search[S <: Solution](problem: Problem[S]): S = {
    var solution = problem.randomSolution()
    var bestSolution = solution
    chrono.start()
    while (!chrono.willOutOfTime) {
      solution = selectionRange
        .map(_ => problem.tweakSolution(solution))
        .maxBy(_.quality)
      if (solution.quality > bestSolution.quality) {
        bestSolution = solution
      }
    }
    bestSolution
  }

}
