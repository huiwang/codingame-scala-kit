package com.truelaurel.algorithm.metaheuristic.genetic

/**
  * We don't use case class because neither hashcode nor equal is needed
  */
final class AssessedSolution[S](val solution: S, val quality: Double)
