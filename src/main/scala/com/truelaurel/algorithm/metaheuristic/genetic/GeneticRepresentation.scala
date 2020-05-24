package com.truelaurel.algorithm.metaheuristic.genetic

trait GeneticRepresentation[S] {

  def randomSolution: S

  def crossover(solutionA: S, solutionB: S): (S, S)

  def mutate(solution: S): S

  def assess(solution: S): AssessedSolution[S]
}
