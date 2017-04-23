package com.truelaurel.codingame.caribbean.best

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.caribbean.common.{FixedCabribbeanPlayer, _}
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
case class BestCaribbeanPlayer(me: Int, other: Int,
                           stopper: Stopper = new Chronometer(Duration(46, TimeUnit.MILLISECONDS))
                          ) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(me)
    if (state.barrels.isEmpty && myShips.size > 1) {
      val weakest = myShips.minBy(_.rums)
      val closestToWeakest = myShips.filter(_.id != weakest.id).map(s => weakest.center.distanceTo(s.center)).min
      if (closestToWeakest <= 3) {
        myShips.map(s => if (s.id == weakest.id) Fire(s.id, weakest.nextCenter.toOffset) else Wait(s.id))
      } else {
        simule(state)
      }
    } else {
      simule(state)
    }
  }

  private def simule(state: CaribbeanState) = {
    val muToLambda = new MuPlusLambda(1, 3, stopper)
    val solution = muToLambda.search(BestCaribbeanProblem(me, other, FixedCabribbeanPlayer(other, me), state))
    solution.toActions
  }
}

case class BestCaribbeanProblem(me: Int,
                                other: Int,
                                otherPlayer: GamePlayer[CaribbeanState, CaribbeanAction],
                                state: CaribbeanState) extends Problem[BestCaribbeanSolution] {

  private val convolution = new BoundedVectorConvolution(0.9, 0, 10)
  private val roundsToSimulate = 3
  val rounds: Range = 0 until roundsToSimulate
  val actionLength: Int = state.shipsOf(me).size
  val chromosome: Range = 0 until (roundsToSimulate * actionLength)

  override def randomSolution(): BestCaribbeanSolution = {
    BestCaribbeanSolution(this, chromosome.map(_ => NoiseGenerators.uniform(5, 5).apply()).toVector)
  }

  override def tweakSolution(solution: BestCaribbeanSolution): BestCaribbeanSolution = {
    solution.copy(actions = convolution.tweak(solution.actions,
      NoiseGenerators.gaussian(mean = 0, stdDerivation = 1)))
  }

}

case class BestCaribbeanSolution(problem: BestCaribbeanProblem,
                                 actions: Vector[Double]) extends Solution {
  val quality: Double = {
    val simulatedState = targetState()

    val otherShips = simulatedState.shipsOf(problem.other)

    val myShips = simulatedState.shipsOf(problem.me)


    val otherScore = 0.001 * otherShips.map(ship => {
      ship.rums
    }).sum

    if (problem.state.barrels.isEmpty && myShips.size > 1 && myShips.exists(_.rums < 50)) {
      myShips.map(ship => {
        val shipValues = myShips.map(ms => {
          ms.rums * Math.pow(0.5, ms.center.distanceTo(ship.center))
        }).sum
        val freeHex = CaribbeanContext.reachable(ship.center).size
        ship.rums + 0.0001 * shipValues
      }).sum - otherScore
    } else {
      myShips.map(ship => {
        val barrelValues = simulatedState.barrels.values
          .map(b => b.rums * Math.pow(0.95, b.cube.distanceTo(ship.center))).sum
        val freeHex = CaribbeanContext.reachable(ship.center).size
        ship.rums + 0.001 * barrelValues + 0.0001 * freeHex
      }).sum - otherScore
    }
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
        case _ if x > 9 || x < 1 => Port(shipId)
        case _ if x > 8 || x < 2 => Starboard(shipId)
        case _ if x > 7 || x < 3 => Faster(shipId)
        case _ if x > 6 || x < 4 => Slower(shipId)
        case y =>
          val ship = myShips(i)
          val targets: Vector[Cube] = state.ships.values
            .filter(s => s.id != ship.id
              && (s.owner != problem.me)
              && ship.nextCenter.distanceTo(s.center) <= CaribbeanContext.fireMaxDistance).map(_.nextCenter).toVector
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
