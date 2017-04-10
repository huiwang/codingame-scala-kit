package com.truelaurel.codingame.metaheuristic.simulatedannealing

import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.time.Chronometer

import scala.concurrent.duration.Duration
import scala.util.Random

class SimulatedAnnealing(duration: Duration, initTemperature: Double, coolingRate: Double) {
  val chronometer = new Chronometer(duration)
  val random = Random

  def search[S <: Solution](problem: Problem[S]): S = {
    var solution = problem.randomSolution()
    var best = solution
    var t = initTemperature
    chronometer.start()
    while (!problem.isGoodEnough(solution) && !chronometer.isRunOut) {
      val tweaked = problem.tweakSolution(solution)
      if (tweaked.quality() > solution.quality() ||
        random.nextDouble() < Math.exp((tweaked.quality() - solution.quality()) / t)) {
        solution = tweaked
      }

      if (solution.quality() > best.quality()) {
        best = solution
      }

      t *= coolingRate
    }
    best
  }

}
