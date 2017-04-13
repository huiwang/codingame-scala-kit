package com.truelaurel.codingame.csb.head

import java.util.concurrent.TimeUnit

import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.common.{AngleThrust, PodAction, StrikeBackGameState}
import com.truelaurel.codingame.csb.offline.StrikeBackArena
import com.truelaurel.codingame.engine.GamePlayer
import com.truelaurel.codingame.math.Mathl
import com.truelaurel.codingame.metaheuristic.evolutionstrategy.MuToLambda
import com.truelaurel.codingame.metaheuristic.model.{Problem, Solution}

import scala.concurrent.duration.Duration

/**
  * Created by hwang on 02/04/2017.
  */
case class StrikeBackPlayer(range: Vector[Int],
                            otherRange: Vector[Int]
                           ) extends GamePlayer[StrikeBackGameState, PodAction] {

  override def reactTo(state: StrikeBackGameState): Vector[PodAction] = {
    val muToLambda = new MuToLambda(100, 20, Duration(90, TimeUnit.MILLISECONDS))
    val solution = muToLambda.search(StrikeBackProblem(range, otherRange, BestStrikeBackPlayer(otherRange), state))
    solution.actions.take(2)
  }
}

case class StrikeBackProblem(myRange: Vector[Int],
                             otherRange: Vector[Int],
                             otherPlayer: GamePlayer[StrikeBackGameState, PodAction],
                             state: StrikeBackGameState) extends Problem[StrikeBackSolution] {

  override def randomSolution(): StrikeBackSolution = {
    val actions: Vector[PodAction] = (0 until 6).flatMap(_ => myRange.map(pod => {
      val podDisk = state.pods(pod)
      val podAngle = state.angles(pod)
      AngleThrust(podDisk.p, podAngle, generateRotate(), generateThrust())
    })).toVector
    StrikeBackSolution(myRange, otherRange, state, otherPlayer, actions)
  }

  private def generateRotate(): Int = {
    val p = Mathl.random.nextDouble()
    if (p < 0.4) 18 else if (p < 0.8) -18 else 0
  }

  private def generateThrust(): Int = {
    val p = Mathl.random.nextDouble()
    if (p < 0.6) 200 else if (p < 0.9) 100 else 0
  }

  override def tweakSolution(solution: StrikeBackSolution): StrikeBackSolution = {
    solution
  }

}

case class StrikeBackSolution(myRange: Vector[Int],
                              otherRange: Vector[Int],
                              state: StrikeBackGameState,
                              other: GamePlayer[StrikeBackGameState, PodAction],
                              actions: Vector[PodAction]) extends Solution {

  lazy val quality: Double = {
    val targetState = (0 until 6).foldLeft(state)((s, r) => {
      val podActions = actions.slice(r * 2, r * 2 + 2)
      StrikeBackArena.next(s, podActions ++ other.reactTo(s))
    })
    val myBest = myRange.maxBy(i => {
      val nextCP = targetState.nextCPs(i)
      nextCP * 100.0 - (targetState.checkPoints(nextCP).p - targetState.pods(i).p).mag
    })
    val otherBest = otherRange.maxBy(i => {
      val nextCP = targetState.nextCPs(i)
      nextCP * 100.0 - (targetState.checkPoints(nextCP % state.checkPoints.size).p - targetState.pods(i).p).mag
    })
    myBest
  }
}



