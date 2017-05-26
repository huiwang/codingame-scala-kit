package com.truelaurel.codingame.metaheuristic.genetic

import com.truelaurel.codingame.math.Mathl
import com.truelaurel.codingame.time.Stopper

import scala.collection.mutable.ArrayBuffer

class GeneticAlgorithm(popSize: Int, tournamentSize: Int, stopper: Stopper) {
  require(popSize % 2 == 0, "population size must be even")
  require(tournamentSize > 0, "tournament size must be greater than zero")

  def search[S <: GeneticSolution](problem: GeneticProblem[S]): S = {
    stopper.start()
    var parents = ArrayBuffer.fill(popSize)(problem.randomSolution)
    var children = ArrayBuffer.fill(popSize)(null.asInstanceOf[S])
    var bestSoFar = null.asInstanceOf[S]

    while (!stopper.willOutOfTime) {
      val bestInGeneration = parents.maxBy(_.quality)

      if (bestSoFar == null || bestInGeneration.quality > bestSoFar.quality) {
        bestSoFar = bestInGeneration
      }

      //build a new generation from crossover and mutation
      var i = 0

      while (i < popSize) {
        val parentA = problem.copy(tournamentSelect(parents))
        val parentB = problem.copy(tournamentSelect(parents))
        val (childA, childB) = problem.crossover(parentA, parentB)
        children(i) = problem.mutate(childA)
        children(i + 1) = problem.mutate(childB)
        i = i + 2
      }

      val tmp = parents
      parents = children
      children = tmp

    }
    bestSoFar
  }

  private def tournamentSelect[S <: GeneticSolution](population: ArrayBuffer[S]) = {
    def pickRandomIndividual = population(Mathl.random.nextInt(popSize))

    var best = pickRandomIndividual
    var i = 2
    while (i <= tournamentSize) {
      val next = pickRandomIndividual
      if (next.quality > best.quality) best = next
      i = i + 1
    }
    best
  }

}
