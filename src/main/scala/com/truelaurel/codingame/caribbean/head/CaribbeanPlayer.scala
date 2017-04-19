package com.truelaurel.codingame.caribbean.head

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.caribbean.best.WaitingCabribbeanPlayer
import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.offline.CaribbeanArena
import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.hexagons.Cube
import com.truelaurel.codingame.metaheuristic.evolutionstrategy.MuPlusLambda
import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.metaheuristic.tweak.{BoundedVectorConvolution, NoiseGenerators}

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 14/04/2017.
  */
case class CaribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val muToLambda = new MuPlusLambda(2, 4, Duration(45, TimeUnit.MILLISECONDS))
    val solution = muToLambda.search(CaribbeanProblem(playerId, otherPlayer, WaitingCabribbeanPlayer(otherPlayer, playerId), state))
    solution.toActions
  }
}

case class CaribbeanProblem(me: Int,
                            other: Int,
                            otherPlayer: GamePlayer[CaribbeanState, CaribbeanAction],
                            state: CaribbeanState) extends Problem[CaribbeanSolution] {

  private val convolution = new BoundedVectorConvolution(0.3, 0, 10)
  private val roundsToSimulate = 4
  val rounds: Range = 0 until roundsToSimulate
  val actionLength: Int = state.shipsOf(me).size
  val chromosome: Range = 0 until (roundsToSimulate * actionLength)

  override def randomSolution(): CaribbeanSolution = {
    CaribbeanSolution(this, chromosome.map(_ => NoiseGenerators.uniform(5, 5).apply()).toVector)
  }

  override def tweakSolution(solution: CaribbeanSolution): CaribbeanSolution = {
    solution.copy(actions = convolution.tweak(solution.actions,
      NoiseGenerators.gaussian(mean = 0, stdDerivation = 2)))
  }

}

case class CaribbeanSolution(problem: CaribbeanProblem,
                             actions: Vector[Double]) extends Solution {
  override def quality(): Double = {
    val simulatedState = targetState()

    val myScore = simulatedState.shipsOf(problem.me).map(ship => {
      100000 * ship.rums +
        ship.speed +
        problem.state.barrels.map(b => b.rums * Math.pow(0.95, b.cube.distanceTo(ship.center))).sum
    }).sum

    val otherScore = simulatedState.shipsOf(problem.other).map(ship => {
      100000 * ship.rums
    }).sum

    myScore - otherScore
  }

  def targetState(): CaribbeanState = {
    problem.rounds.foldLeft(problem.state)((s, r) => {
      val shipActions = actions.slice(r * problem.actionLength, (r + 1) * problem.actionLength)
      val adapted = adapt(s, shipActions)
      CaribbeanArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }

  def adapt(state: CaribbeanState, shipActions: Vector[Double]): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(problem.me)
    myShips.indices.map(i => {
      val shipId = myShips(i).id
      val x = shipActions(i)
      x match {
        case _ if x > 8 => Port(shipId)
        case _ if x > 6 => Starboard(shipId)
        case _ if x > 4 => Faster(shipId)
        case _ if x > 3 => Slower(shipId)
        case y =>
          val ship = myShips(i)
          val targetShips = state.ships.filter(s => s.owner != ship.owner && s.center.distanceTo(ship.center) <= CaribbeanContext.fireMaxDistance).map(_.center)
          val targets: Vector[Cube] = targetShips.flatMap(cube => CaribbeanContext.cubeToNeighbors(cube) + cube)
          if (targets.isEmpty) Wait(shipId) else {
            val target = targets((y * 100).toInt % targets.size)
            Fire(shipId, target.toOffset)
          }
      }
    }).toVector
  }

  def toActions: Vector[CaribbeanAction] = {
    adapt(problem.state, actions.take(problem.actionLength))
  }
}
