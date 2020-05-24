package com.truelaurel.algorithm.metaheuristic.hillclimbing

import com.truelaurel.algorithm.metaheuristic.model.{Problem, Solution}
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.Duration

/**
  * Replace next starting solution directly with the tweaked solution
  * This allows to make more exploration even the tweaked one is not better
  */
class HillClimbingWithReplacement(duration: Duration) {

  val chrono = new Chronometer(duration)

  def search[S <: Solution](problem: Problem[S]): S = {
    var solution = problem.randomSolution()
    var bestSolution = solution
    chrono.start()
    while (!chrono.willOutOfTime) {
      solution = problem.tweakSolution(solution)
      if (solution.quality > bestSolution.quality) {
        bestSolution = solution
      }
    }
    bestSolution
  }
}
