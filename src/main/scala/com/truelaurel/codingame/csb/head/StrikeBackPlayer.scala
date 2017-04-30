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
  private val myRole = state.role(me)
  val myOriDefenser: Pod = myRole(0)
  val myOriRacer: Pod = myRole(1)

  private val otherRole = state.role(me)
  val otherOriDefenser: Pod = otherRole(0)
  val otherOriRacer: Pod = otherRole(1)

  val otherRacingToOtherGoal: Double = PodAnalysis.distanceToGoal(otherOriRacer, otherOriRacer, state)
  val myDefenseToOtherGoal: Double = PodAnalysis.distanceToGoal(myOriDefenser, otherOriRacer, state)


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
    val myPods = state.role(problem.me)
    val myDefense = myPods(0)
    val myRacing = myPods(1)
    val otherPods = state.role(problem.other)
    val otherDefense = otherPods(0)
    val otherRacing = otherPods(1)

    val checkPointScore = Math.pow(2, myRacing.goal - problem.state.pods(myRacing.id).goal)
    val toNextCheckPointScore = Math.pow(0.999, (state.checkPoint(myRacing.goal) - myRacing.position).mag)
    val racingScore = checkPointScore + toNextCheckPointScore

    val towardOtherRacer = myDefense.angle.dotProduct((otherRacing.position - myDefense.position).norm)
    val defenseScore = if (problem.myDefenseToOtherGoal < problem.otherRacingToOtherGoal) {
      0.0001 * towardOtherRacer -
        Math.pow(0.99, PodAnalysis.distanceToGoal(otherRacing, otherRacing, state)) +
        0.1 * Math.pow(0.99, PodAnalysis.distanceToGoal(myDefense, otherRacing, state))

    } else {
      0.0001 * towardOtherRacer + Math.pow(0.999, 400.0.max(PodAnalysis.distanceToNextGoal(myDefense, problem.otherOriRacer.goal, state)))
    }

    val coolDownScore = -(state.context.shieldCoolDown(myDefense.id) + state.context.shieldCoolDown(myRacing.id))

    racingScore + 0.5 * defenseScore + 0.00001 * coolDownScore
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
        case x if x > 0.95 => Shield(p.id, p.position, p.angle, angle)
        case x if x > 0.5 => AngleThrust(p.id, p.position, p.angle, angle, 200)
        case x if x > 0.3 => AngleThrust(p.id, p.position, p.angle, angle, 100)
        case x if x > 0.1 => AngleThrust(p.id, p.position, p.angle, angle, 50)
        case _ => AngleThrust(p.id, p.position, p.angle, angle, 0)
      }
    })
  }

}