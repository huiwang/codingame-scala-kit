package com.truelaurel.codingame.csb.head

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.collision.Collision
import com.truelaurel.codingame.csb.analysis.PodAnalysis
import com.truelaurel.codingame.csb.arena.StrikeBackArena
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.model._
import com.truelaurel.codingame.game.GamePlayer
import com.truelaurel.codingame.metaheuristic.evolutionstrategy.MuPlusLambda
import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.metaheuristic.tweak.{BoundedVectorConvolution, NoiseGenerators}
import com.truelaurel.codingame.time.Chronometer

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 02/04/2017.
  */
case class StrikeBackPlayer(me: Int, other: Int) extends GamePlayer[StrikeBackState, StrikeBackAction] {

  override def reactTo(state: StrikeBackState): Vector[StrikeBackAction] = {
    val muToLambda = new MuPlusLambda(5, 10, new Chronometer(Duration(145, TimeUnit.MILLISECONDS)))
    val solution = muToLambda.search(StrikeBackProblem(me, other, BestStrikeBackPlayer(StrikeBackContext.other), state))
    solution.adapt(state, solution.actions.take(2))
  }
}

case class StrikeBackProblem(me: Int,
                             other: Int,
                             otherPlayer: GamePlayer[StrikeBackState, StrikeBackAction],
                             state: StrikeBackState) extends Problem[StrikeBackSolution] {

  private val convolution = new BoundedVectorConvolution(0.9, -180, +180)
  val pods: Int = 2
  val actionPerPod: Int = 3
  val rounds: Int = 6
  val actionLength: Int = pods * actionPerPod
  val roundRange: Range = 0 until rounds
  val chromosome: Range = 0 until rounds * actionLength

  override def randomSolution(): StrikeBackSolution = {
    StrikeBackSolution(this, chromosome.map(_ => NoiseGenerators.uniform(180).apply()).toVector)
  }

  override def tweakSolution(solution: StrikeBackSolution): StrikeBackSolution = {
    solution.copy(actions = convolution.tweak(solution.actions, NoiseGenerators.gaussian(0, 500)))
  }

}

case class StrikeBackSolution(problem: StrikeBackProblem,
                              actions: Vector[Double]) extends Solution {
  lazy val quality: Double = {
    val myPods = state.podsOf(problem.me).sortBy(_.goal)
    val myDefense = myPods(0)
    val myRacing = myPods(1)
    val otherPods = state.podsOf(problem.other).sortBy(_.goal)
    val otherDefense = otherPods(0)
    val otherRacing = otherPods(1)

    val racingScore = myRacing.goal * 10000000.0 - (state.checkPoint(myRacing.goal) - myRacing.position).mag
    val collisionScore = Math.pow(0.8, PodAnalysis.collisionTime(myDefense, otherRacing).getOrElse(Double.MaxValue))
    val readyScore = (state.checkPoint(otherRacing.goal) - myDefense.position).mag
    val blockingScore = (state.checkPoint(otherRacing.goal) - otherRacing.position).mag
    val defenseScore = blockingScore + collisionScore - readyScore

    racingScore + defenseScore
  }

  lazy val state: StrikeBackState = {
    problem.roundRange.foldLeft(problem.state)((s, r) => {
      val podActions = actions.slice(r * problem.actionLength, r * problem.actionLength + problem.actionLength)
      val adapted = adapt(s, podActions)
      StrikeBackArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }

  def adapt(current: StrikeBackState, podActions: Vector[Double]): Vector[StrikeBackAction] = {
    current.podsOf(problem.me).map(p => {
      val start = (p.id * problem.actionPerPod) % problem.actionLength
      val angle = actions(start)
      val thrust = actions(start + 1)
      val shield = actions(start + 2)
      if (shield + 180 > 350) Shield(p.id, p.position, p.angle, angle / 10) else {
        AngleThrust(p.id, p.position, p.angle, angle / 10, (thrust + 180) / 1.8)
      }
    })
  }

}