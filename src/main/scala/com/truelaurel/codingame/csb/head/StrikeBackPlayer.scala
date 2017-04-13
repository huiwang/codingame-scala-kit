package com.truelaurel.codingame.csb.head

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.common.{AngleThrust, PodAction, StrikeBackGameState}
import com.truelaurel.codingame.csb.offline.StrikeBackArena
import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.math.Mathl
import com.truelaurel.codingame.metaheuristic.evolutionstrategy.MuPlusLambda
import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}
import com.truelaurel.codingame.metaheuristic.tweak.{BoundedVectorConvolution, NoiseGenerators}

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 02/04/2017.
  */
case class StrikeBackPlayer(range: Vector[Int],
                            otherRange: Vector[Int]
                           ) extends GamePlayer[StrikeBackGameState, PodAction] {

  override def reactTo(state: StrikeBackGameState): Vector[PodAction] = {
    val muToLambda = new MuPlusLambda(100, 20, Duration(90, TimeUnit.MILLISECONDS))
    val solution = muToLambda.search(StrikeBackProblem(range, otherRange, BestStrikeBackPlayer(otherRange), state))
    solution.adapt(state, solution.actions.take(2))
  }
}

case class StrikeBackProblem(myRange: Vector[Int],
                             otherRange: Vector[Int],
                             otherPlayer: GamePlayer[StrikeBackGameState, PodAction],
                             state: StrikeBackGameState) extends Problem[StrikeBackSolution] {

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
    best(problem.myRange, targetState)
  }

  lazy val targetState: StrikeBackGameState = {
    problem.rounds.foldLeft(problem.state)((s, r) => {
      val podActions = actions.slice(r * 2, r * 2 + 2)
      val adapted = adapt(s, podActions)
      StrikeBackArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }

  def adapt(s: StrikeBackGameState, podActions: Vector[Double]): Vector[AngleThrust] = {
    problem.myRange.map(i => AngleThrust(s.pods(i).p, s.angles(i), actions.head / 10, (actions.last + 180) / 1.8))
  }

  private def best(range: Vector[Int], targetState: StrikeBackGameState) = {
    range.map(i => {
      val nextCP = targetState.nextCPs(i)
      nextCP * 10000000.0 - (targetState.checkPoints(nextCP % problem.state.checkPoints.size).p - targetState.pods(i).p).mag
    }).max
  }
}