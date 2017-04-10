package com.truelaurel.codingame.metaheuristic.hillclimbing

import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.time.Chronometer

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 12/02/2017.
  */
class SteepestAscentHillClimbingWithReplacement(duration: Duration, selectionPressure: Int)  {

  val chrono = new Chronometer(duration)
  val selectionRange: Range = 0 until selectionPressure

  def search[S <: Solution](problem: Problem[S]): S = {
    var solution = problem.randomSolution()
    var bestSolution = solution
    chrono.start()
    while (!problem.isGoodEnough(solution) && !chrono.isRunOut) {
      solution = selectionRange.map(_ => problem.tweakSolution(solution)).maxBy(_.quality())
      if (solution.quality() > bestSolution.quality()) {
        bestSolution = solution
      }
    }
    bestSolution
  }


}
