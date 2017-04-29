package com.truelaurel.codingame.csb.head

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.csb.arena.StrikeBackArena
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.model.{AngleThrust, StrikeBackAction, StrikeBackContext, StrikeBackState}
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
    val muToLambda = new MuPlusLambda(20, 100, new Chronometer(Duration(90, TimeUnit.MILLISECONDS)))
    val solution = muToLambda.search(StrikeBackProblem(me, other, BestStrikeBackPlayer(StrikeBackContext.other), state))
    solution.adapt(state, solution.actions.take(2))
  }
}

case class StrikeBackProblem(me: Int,
                             other: Int,
                             otherPlayer: GamePlayer[StrikeBackState, StrikeBackAction],
                             state: StrikeBackState) extends Problem[StrikeBackSolution] {

  private val convolution = new BoundedVectorConvolution(0.3, -180, +180)
  val chromosome: Range = 0 until 12
  val rounds: Range = 0 until 6

  override def randomSolution(): StrikeBackSolution = {
    StrikeBackSolution(this, chromosome.map(_ => NoiseGenerators.uniform(180).apply()).toVector)
  }

  override def tweakSolution(solution: StrikeBackSolution): StrikeBackSolution = {
    solution.copy(actions = convolution.tweak(solution.actions, NoiseGenerators.uniform(180)))
  }

}

case class StrikeBackSolution(problem: StrikeBackProblem,
                              actions: Vector[Double]) extends Solution {
  lazy val quality: Double = {
    val myPods = targetState.podsOf(problem.me)
    myPods.map(pod => {
      pod.goal * 10000000.0 - (targetState.checkPoint(pod.goal) - pod.position).mag
    }).max
  }

  lazy val targetState: StrikeBackState = {
    problem.rounds.foldLeft(problem.state)((s, r) => {
      val podActions = actions.slice(r * 2, r * 2 + 2)
      val adapted = adapt(s, podActions)
      StrikeBackArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }

  def adapt(s: StrikeBackState, podActions: Vector[Double]): Vector[AngleThrust] = {
    s.podsOf(problem.me).map(p => AngleThrust(p.id, p.position, p.angle, actions.head / 10, (actions.last + 180) / 1.8))
  }

}