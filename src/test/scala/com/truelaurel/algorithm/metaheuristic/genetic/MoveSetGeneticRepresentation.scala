package com.truelaurel.algorithm.metaheuristic.genetic

import scala.util.Random

case class MoveSetGeneticRepresentation(ref: Referee = null)
    extends GeneticRepresentation[MoveSet] {
  override def mutate(solution: MoveSet): MoveSet = {
    for (i <- 0 until 20) {
      if (Random.nextInt.abs % 100 < 10)
        solution.vecarr(i) = Vec(Random.nextInt.abs % 200, Random.nextInt % 180)
    }
    solution
  }

  override def randomSolution: MoveSet = {
    val vecarr = new Array[Vec](20)
    for (i <- 0 until 20)
      vecarr(i) = Vec(Random.nextInt.abs % 200, Random.nextInt % 180)
    MoveSet(vecarr)
  }

  override def crossover(
      solutionA: MoveSet,
      solutionB: MoveSet
  ): (MoveSet, MoveSet) = {
    val vecarrA = new Array[Vec](20)
    val vecarrB = new Array[Vec](20)
    for (i <- 0 until 20) {
      val vA = solutionA.vecarr(i)
      val sA = vA.speed
      val rA = vA.rotate
      val vB = solutionB.vecarr(i)
      val sB = vB.speed
      val rB = vB.rotate
      val nsA = (sA + Random.nextDouble * (sB - sA)).toInt
      val nrA = (rA + Random.nextDouble * (rB - rA)).toInt
      vecarrA(i) = Vec(nsA, nrA)
      val nsB = (sA + Random.nextDouble * (sB - sA)).toInt
      val nrB = (rA + Random.nextDouble * (rB - rA)).toInt
      vecarrB(i) = Vec(nsB, nrB)
    }
    (MoveSet(vecarrA), MoveSet(vecarrB))
  }

  override def assess(solution: MoveSet): AssessedSolution[MoveSet] =
    new AssessedSolution[MoveSet](solution, ref.evaluate(solution))
}
