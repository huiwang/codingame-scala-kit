package com.truelaurel.algorithm.metaheuristic.genetic

import com.truelaurel.math.Mathl
import com.truelaurel.time.Stopper

class GeneticAlgorithm[S](popSize: Int, tournamentSize: Int, eliteSize: Int, stopper: Stopper) {
  require(popSize % 2 == 0, "population size must be even")
  require(eliteSize % 2 == 0, "elite size must be even")
  require(tournamentSize > 0, "tournament size must be greater than zero")

  private val fullPop = (0 until popSize).toVector
  private val halfPop = (0 until (popSize - eliteSize)/ 2).toVector

  def search(repr: GeneticRepresentation[S]): S = {
    stopper.start()
    var parents = fullPop.map(_ => repr.assess(repr.randomSolution)).sortBy(_.quality)
    var best = null.asInstanceOf[AssessedSolution[S]]

    while (!stopper.willOutOfTime) {
      best = better(best, parents.last)

      parents = (halfPop.flatMap(_ => {
        val parentA = tournamentSelect(parents)
        val parentB = tournamentSelect(parents)
        val (childA, childB) = repr.crossover(parentA, parentB)
        val mutatedA = repr.mutate(childA)
        val mutatedB = repr.mutate(childB)
        val assessedA = repr.assess(mutatedA)
        val assessedB = repr.assess(mutatedB)
        Vector(assessedA, assessedB)
      }) ++ parents.takeRight(eliteSize)).sortBy(_.quality)
    }
    best.solution
  }

  private def better(bestSoFar: AssessedSolution[S], bestInGeneration: AssessedSolution[S]) = {
    if (bestSoFar == null || bestInGeneration.quality > bestSoFar.quality) bestInGeneration else bestSoFar
  }

  private def tournamentSelect(population: Vector[AssessedSolution[S]]) = {
    var best = pickRandomIndividual(population)
    var i = 2
    while (i <= tournamentSize) {
      val next = pickRandomIndividual(population)
      if (next.quality > best.quality) best = next
      i = i + 1
    }
    best.solution
  }

  private def pickRandomIndividual[T](population: Vector[T]) = {
    population(Mathl.random.nextInt(popSize))
  }


}
