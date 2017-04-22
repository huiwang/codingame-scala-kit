package com.truelaurel.codingame.caribbean.best

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.offline.CaribbeanArena
import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.hexagons.Cube
import com.truelaurel.codingame.metaheuristic.evolutionstrategy.MuPlusLambda
import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.metaheuristic.tweak.{BoundedVectorConvolution, NoiseGenerators}
import com.truelaurel.codingame.time.{Chronometer, Stopper}

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 14/04/2017.
  */
case class BestCaribbeanPlayer(playerId: Int, otherPlayer: Int,
                               stopper: Stopper = new Chronometer(Duration(45, TimeUnit.MILLISECONDS)))
  extends GamePlayer[CaribbeanState, CaribbeanAction] {

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val muToLambda = new MuPlusLambda(2, 4, stopper)
    val solution = muToLambda.search(BestCaribbeanProblem(playerId, otherPlayer, FixedCabribbeanPlayer(otherPlayer, playerId), state))
    solution.toActions
  }
}

case class BestCaribbeanProblem(me: Int,
                                other: Int,
                                otherPlayer: GamePlayer[CaribbeanState, CaribbeanAction],
                                state: CaribbeanState) extends Problem[BestCaribbeanSolution] {

  private val convolution = new BoundedVectorConvolution(0.3, 0, 10)
  private val roundsToSimulate = 4
  val rounds: Range = 0 until roundsToSimulate
  val actionLength: Int = state.shipsOf(me).size
  val chromosome: Range = 0 until (roundsToSimulate * actionLength)

  override def randomSolution(): BestCaribbeanSolution = {
    BestCaribbeanSolution(this, chromosome.map(_ => NoiseGenerators.uniform(5, 5).apply()).toVector)
  }

  override def tweakSolution(solution: BestCaribbeanSolution): BestCaribbeanSolution = {
    solution.copy(actions = convolution.tweak(solution.actions,
      NoiseGenerators.gaussian(mean = 0, stdDerivation = 2)))
  }

}

case class BestCaribbeanSolution(problem: BestCaribbeanProblem,
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
          val targets: Vector[Cube] = targetShips.flatMap(cube => CaribbeanContext.neighbors(cube) + cube)
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
