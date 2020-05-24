package com.truelaurel.algorithm.metaheuristic.genetic

import com.truelaurel.time.CountStopper
import org.scalatest.{FlatSpec, Matchers}

class GeneticAlgorithmTest extends FlatSpec with Matchers {

  behavior of "GeneticAlgorithm"

  val wps: Array[WayPoint] = Array(
    new WayPoint(0, 0),
    new WayPoint(1000, 0)
  )
  val ref = Referee(wps)
  val msgr = MoveSetGeneticRepresentation(ref)

  val popSize = 100
  val tournamentSize = 20
  val eliteSize = 20
  val stopper = new CountStopper(5000)

  val ga =
    new GeneticAlgorithm[MoveSet](popSize, tournamentSize, eliteSize, stopper)

  it can "search for a good result" in {
    val result = ga.search(msgr)
    ref.evaluate(result) should be >= 2000.0
  }

}
