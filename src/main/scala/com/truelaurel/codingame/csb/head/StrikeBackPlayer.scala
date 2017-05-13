package com.truelaurel.codingame.csb.head

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.csb.analysis.PodAnalysis
import com.truelaurel.codingame.csb.arena.StrikeBackArena
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.model._
import com.truelaurel.codingame.game.GamePlayer
import com.truelaurel.codingame.metaheuristic.evolutionstrategy.MuPlusLambda
import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.metaheuristic.tweak.{BoundedVectorConvolution, NoiseGenerators}
import com.truelaurel.codingame.time.{Chronometer, Stopper}

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 02/04/2017.
  */
case class StrikeBackPlayer(me: Int, other: Int,
                            stopper: Stopper = new Chronometer(Duration(145, TimeUnit.MILLISECONDS))) extends GamePlayer[StrikeBackState, StrikeBackAction] {

  override def reactTo(state: StrikeBackState): Vector[StrikeBackAction] = {
    val muToLambda = new MuPlusLambda(5, 10, stopper)
    val solution = muToLambda.search(StrikeBackProblem(me, other, BestStrikeBackPlayer(StrikeBackContext.other), state))
    solution.decode(state, solution.actions.take(2))
  }
}

case class StrikeBackProblem(me: Int,
                             other: Int,
                             otherPlayer: GamePlayer[StrikeBackState, StrikeBackAction],
                             state: StrikeBackState) extends Problem[StrikeBackSolution] {

  private val convolution = new BoundedVectorConvolution(0.9, 0, 1.0)
  val pods: Int = 2
  val actionPerPod: Int = 2
  val rounds: Int = 6
  val actionLength: Int = pods * actionPerPod
  val roundRange: Range = 0 until rounds
  val chromosome: Range = 0 until rounds * actionLength
  val Vector(myBlocker, myRacer) = state.role(me)
  val Vector(_, otherRacer) = state.role(other)
  val holdingGate = 6
  private val blockerToRacerCollisionTime = PodAnalysis.podToPodCollisionTime(myBlocker, otherRacer)
  val shouldBlock: Boolean = PodAnalysis.pivotTo(myBlocker, otherRacer) > 0 &&
    blockerToRacerCollisionTime.getOrElse(Double.MaxValue) <= holdingGate

  val shouldPivot: Boolean =
    PodAnalysis.podToOtherGoalDistance(myBlocker, otherRacer, state) < PodAnalysis.podToGoalDistance(otherRacer, state) &&
      blockerToRacerCollisionTime.isEmpty

  override def randomSolution(): StrikeBackSolution = {
    StrikeBackSolution(this, chromosome.map(_ => NoiseGenerators.uniform(0.5, 0.5).apply()).toVector)
  }

  override def tweakSolution(solution: StrikeBackSolution): StrikeBackSolution = {
    solution.copy(actions = convolution.tweak(solution.actions, NoiseGenerators.gaussian(0, 3)))
  }

}

case class StrikeBackSolution(problem: StrikeBackProblem,
                              actions: Vector[Double]) extends Solution {
  lazy val quality: Double = {

    val Vector(myBlocker, myRacer) = state.role(problem.me)
    val Vector(_, otherRacer) = state.role(problem.other)

    val myRacerCPBonus = computeCPScore(myRacer)
    val myRacerGoalBonus = computeRacerGoalScore(myRacer)
    val otherRacerCPMalus = computeCPScore(otherRacer)
    val otherRacerGoalMalus = computeRacerGoalScore(otherRacer)

    val myBlockerTowardOtherRacerBonus = PodAnalysis.pivotTo(myBlocker, otherRacer)
    val myBlockerSpeedBonus = otherRacer.speed.norm.dotProduct(myBlocker.angle)
    val myBlockerToOtherRacerDistanceBonus = approach(PodAnalysis.distance(myBlocker, otherRacer))
    val myBlockerToOtherRacerCollisionBonus = Math.pow(0.99, PodAnalysis.podToPodCollisionTime(myBlocker, otherRacer).getOrElse(Double.MaxValue))
    val myBlockerToOtherRacerGoalBonus = approach(PodAnalysis.podToOtherGoalDistance(myBlocker, otherRacer, state))
    val myBlockerToOtherRacerNextGoalBonus = approach(PodAnalysis.podTodNextGoalDistance(myBlocker, problem.otherRacer.goal, state))

    val myBlockerShieldBonus = state.context.shieldCoolDown(myBlocker.id)

    val coolDownMalus = coolDownScore(myBlocker, myRacer)
    val myRacerScore = 3.0 * (myRacerCPBonus + myRacerGoalBonus)
    if (problem.shouldPivot) {
      myRacerScore + myBlockerToOtherRacerGoalBonus + myBlockerTowardOtherRacerBonus
    } else if (problem.shouldBlock) {
      myRacerScore + myBlockerToOtherRacerCollisionBonus - otherRacerGoalMalus
    } else {
      myRacerScore + myBlockerToOtherRacerNextGoalBonus + 0.0001 * myBlockerTowardOtherRacerBonus
    }
  }

  private def computeRacerGoalScore(racer: Pod) = {
    Math.pow(0.999, PodAnalysis.podToGoalDistance(racer, state))
  }

  private def computeCPScore(racer: Pod) = {
    Math.pow(2, racer.goal - problem.state.pods(racer.id).goal)
  }

  private def coolDownScore(blocker: Pod, racer: Pod) = {
    state.context.shieldCoolDown(blocker.id) + state.context.shieldCoolDown(racer.id)
  }

  private def approach(distance: Double) = {
    Math.pow(0.999, 400.0.max(distance))
  }

  lazy val state: StrikeBackState = {
    problem.roundRange.foldLeft(problem.state)((s, r) => {
      val podActions = actions.slice(r * problem.actionLength, r * problem.actionLength + problem.actionLength)
      val decoded = decode(s, podActions)
      StrikeBackArena.next(s, decoded ++ problem.otherPlayer.reactTo(s))
    })
  }

  def decode(current: StrikeBackState, podActions: Vector[Double]): Vector[StrikeBackAction] = {
    current.podsOf(problem.me).map(p => {
      val start = (p.id * problem.actionPerPod) % problem.actionLength
      val angle = actions(start) match {
        case x if x > 0.7 => 18
        case x if x > 0.6 => 9
        case x if x < 0.3 => -18
        case x if x < 0.4 => -9
        case _ => 0
      }
      val thrust = actions(start + 1)
      thrust match {
        case x if x > 0.9 => Shield(p.id, p.position, p.angle, angle)
        case x if x > 0.5 => AngleThrust(p.id, p.position, p.angle, angle, 200)
        case x if x > 0.3 => AngleThrust(p.id, p.position, p.angle, angle, 100)
        case x if x > 0.1 => AngleThrust(p.id, p.position, p.angle, angle, 50)
        case _ => AngleThrust(p.id, p.position, p.angle, angle, 0)
      }
    })
  }

}