package com.truelaurel.algorithm.metaheuristic.simulatedannealing

import com.truelaurel.algorithm.metaheuristic.model.{Problem, Solution}
import com.truelaurel.fp.Unfold

import scala.concurrent.duration.DurationInt
import scala.math._
import scala.util.Random

// This file demonstrates the usage of the "simulated annealing" algorithm
// for a Mars Lander simple problem (https://www.codingame.com/ide/puzzle/mars-lander-episode-1)
object LanderBasicDemo {
  def main(args: Array[String]): Unit = {
    val initialState =
      LanderBasicState(y = 1000, vy = 100, fuel = 500, power = 0)
    val sol = new SimulatedAnnealing(200.millis, 1000, 0.99)
      .search(initialState.problem)
    println(sol.quality, initialState.endState(sol.moves), sol)
  }
}

case class LanderBasicState(y: Int, vy: Int, fuel: Int, power: Int) {
  state =>

  object problem extends Problem[LanderBasicSolution] {

    def randomSolution(): LanderBasicSolution = {
      val moves = Unfold.unfold(state) { s =>
        if (s.y <= 0 || s.fuel <= 0 || s.y > 3000) None
        else {
          val nextMove = s.genNextMove
          val nextState = s.apply(nextMove)
          Some((nextMove, nextState))
        }
      }
      LanderBasicSolution(moves)
    }

    def tweakSolution(solution: LanderBasicSolution): LanderBasicSolution = {
      val r = Random.nextInt(solution.moves.size)
      val kept = solution.moves.take(r)
      val intermediateState = endState(kept)
      val newEndMoves = intermediateState.problem.randomSolution().moves
      LanderBasicSolution(kept ++ newEndMoves)
    }
  }

  case class LanderBasicSolution(moves: Stream[LanderBasicMove])
      extends Solution {
    val quality: Double = {
      val finalState = endState(moves)
      if (finalState.y > 3000) -finalState.y
      else if (finalState.canLandSafely) 1000 + finalState.fuel
      else finalState.vy
    }
  }

  def endState(movesToApply: Stream[LanderBasicMove]): LanderBasicState =
    movesToApply match {
      case h #:: t if !canLandSafely => apply(h).endState(t)
      case _                         => this
    }

  def canLandSafely: Boolean = y == 0 && vy.abs <= 40

  val angle = 0
  val G = 3.711

  def actualThrust(move: LanderBasicMove): Int =
    if (fuel <= 0) (move.thrust - 1) max 0
    else if (move.thrust > power) (power + 1) min 4
    else if (move.thrust < power) (power - 1) max 0
    else power

  def apply(move: LanderBasicMove): LanderBasicState = {
    // https://www.codingame.com/forum/t/mars-lander-physics-simulator/1459/2
    val thrust = actualThrust(move)
    val ny = y + vy + 0.5 * (cos(angle * Pi / 180) * thrust - G)
    val nvy = vy + (cos(angle * Pi / 180) * thrust - G)
    LanderBasicState(ny.toInt, nvy.toInt, fuel - thrust, thrust)
  }

  val initialMove = LanderBasicMove(power)

  def genNextMove: LanderBasicMove = {
    val r = Random.nextInt(3)
    val move = state.initialMove
    val nextMove =
      if (r == 0 && move.thrust < 4) LanderBasicMove(move.thrust + 1)
      else if (r == 1 && move.thrust > 0) LanderBasicMove(move.thrust - 1)
      else move
    nextMove
  }
}

case class LanderBasicMove(thrust: Int)
