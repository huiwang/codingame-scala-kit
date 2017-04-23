package com.truelaurel.codingame.caribbean.head

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
case class CaribbeanPlayer(me: Int, other: Int,
                           stopper: Stopper = new Chronometer(Duration(47, TimeUnit.MILLISECONDS))
                          ) extends GamePlayer[CaribbeanState, CaribbeanAction] {

  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(me)

    val result = if (state.barrels.isEmpty && myShips.size > 1) {
      val weakest = myShips.minBy(_.rums)
      val distance = myShips.filter(_.id != weakest.id).map(s =>
        CollisionAnalysis.collisionTime(s, weakest.center)).min
      if (distance <= 3 && weakest.rums <= 50 && weakest.speed < 1) {
        myShips.map(s => if (s.id == weakest.id) Fire(s.id, CollisionAnalysis.hitMyself(weakest).toOffset) else Wait(s.id))
      } else {
        simulate(state)
      }
    } else {
      simulate(state)
    }

    val otherShips = state.shipsOf(other)
    val mineable = myShips.find(ship => otherShips.exists(os => CollisionAnalysis.canMine(ship, os)))
    if (mineable.isEmpty) result else {
      val ship = mineable.get
      result.map(r => if (r.shipId == ship.id) MineAction(ship.id) else r)
    }
  }

  private def simulate(state: CaribbeanState) = {
    val muToLambda = new MuPlusLambda(1, 3, stopper)
    val solution = muToLambda.search(CaribbeanProblem(me, other, FixedCabribbeanPlayer(other, me), state))
    solution.toActions
  }
}

case class CaribbeanProblem(me: Int,
                            other: Int,
                            otherPlayer: GamePlayer[CaribbeanState, CaribbeanAction],
                            state: CaribbeanState) extends Problem[CaribbeanSolution] {

  private val convolution = new BoundedVectorConvolution(1.0, 0, 10)
  private val roundsToSimulate = 3
  val rounds: Range = 0 until roundsToSimulate
  val actionLength: Int = state.shipsOf(me).size
  val chromosome: Range = 0 until (roundsToSimulate * actionLength)

  override def randomSolution(): CaribbeanSolution = {
    CaribbeanSolution(this, chromosome.map(_ => NoiseGenerators.uniform(5, 5).apply()).toVector)
  }

  override def tweakSolution(solution: CaribbeanSolution): CaribbeanSolution = {
    solution.copy(actions = convolution.tweak(solution.actions,
      NoiseGenerators.gaussian(mean = 0, stdDerivation = 7)))
  }

}

case class CaribbeanSolution(problem: CaribbeanProblem,
                             actions: Vector[Double]) extends Solution {
  val quality: Double = {
    val simulatedState = targetState()

    val otherShips = simulatedState.shipsOf(problem.other)

    val myShips = simulatedState.shipsOf(problem.me)


    val otherScore = 0.001 * otherShips.map(ship => {
      ship.rums
    }).sum

    if (problem.state.barrels.isEmpty && myShips.size > 1 && myShips.exists(_.rums < 53)) {
      myShips.map(ship => {
        val shipValues = myShips.map(ms => {
          ms.rums * Math.pow(0.5, ms.center.distanceTo(ship.center))
        }).sum
        ship.rums + 0.001 * shipValues
      }).sum - otherScore
    } else {
      myShips.map(ship => {
        val barrelValues = simulatedState.barrels.values
          .map(b => b.rums * Math.pow(0.95, CollisionAnalysis.collisionTime(ship, b.cube))).sum
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
    val otherShips = state.shipsOf(problem.other)
    myShips.indices.map(i => {
      val ship = myShips(i)
      val x = shipActions(i)
      x match {
        case _ if x > 9 || x < 1 => Port(ship.id)
        case _ if x > 8 || x < 2 => Starboard(ship.id)
        case _ if x > 7 || x < 3 => if (ship.speed == 2) Port(ship.id) else Faster(ship.id)
        case _ if x > 6 || x < 4 => if (ship.speed == 0) Starboard(ship.id) else Slower(ship.id)
        case y =>
          val targets: Vector[Cube] = otherShips
            .filter(s => ship.nextCenter.distanceTo(s.bow) <= CaribbeanContext.fireMaxDistance)
            .map(_.nextCenter)
          if (targets.isEmpty) {
            if (y > 5) Port(ship.id) else Starboard(ship.id)
          } else {
            val target = targets((y * 100).toInt % targets.size)
            Fire(ship.id, target.toOffset)
          }
      }
    }).toVector
  }

  def toActions: Vector[CaribbeanAction] = {
    adapt(problem.state, actions.take(problem.actionLength))
  }
}
