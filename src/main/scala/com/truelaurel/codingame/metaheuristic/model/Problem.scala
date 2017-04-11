package com.truelaurel.codingame.metaheuristic.model

/**
  * Representation of a candidate solution. It defines how to initialize and tweak a candidate.
  */
trait Problem[S <: Solution] {

  def randomSolution(): S

  def tweakSolution(solution: S): S

}
