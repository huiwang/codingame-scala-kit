package com.truelaurel.codingame.metaheuristic.genetic

/**
  * Created by hwang on 26/05/2017.
  */

trait GeneticProblem[S <: GeneticSolution] {

  def randomSolution: S

  def copy(solution: S) : S

  def mutate(child: S): S

  def crossover(parentA: S, parentB: S): (S, S)
}
