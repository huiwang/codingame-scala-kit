package com.truelaurel.algorithm.metaheuristic.evolutionstrategy

import com.truelaurel.algorithm.metaheuristic.model.Problem


class RockThrowingProblem extends Problem[RockThrowingSolution] {

  override def randomSolution(): RockThrowingSolution = new RockThrowingSolution

  override def tweakSolution(solution: RockThrowingSolution): RockThrowingSolution = {
    val newsolution = new RockThrowingSolution
    newsolution.setDegree(solution.degree + scala.util.Random.nextDouble * 10 - 5)
    newsolution
  }
}
