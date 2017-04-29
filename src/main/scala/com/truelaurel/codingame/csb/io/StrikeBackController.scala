package com.truelaurel.codingame.csb.io

import com.truelaurel.codingame.csb.model._
import com.truelaurel.codingame.game.GameController
import com.truelaurel.codingame.vectorial.{Vectorl, Vectorls}

import scala.io.StdIn

object StrikeBackController extends GameController[StrikeBackContext, StrikeBackState, StrikeBackAction] {

  override def readContext: StrikeBackContext = {
    val laps = StdIn.readInt()
    val checkpointCount = StdIn.readInt()
    val checkPoints = 0.until(checkpointCount).map(i => {
      val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
      CheckPoint(i, Vectorl(x, y))
    }).toVector
    StrikeBackContext(checkPoints, Vector.empty)
  }

  override def readState(turn: Int, context: StrikeBackContext): StrikeBackState = {
    val pods = StrikeBackContext.podIndices.map(i => {
      val Array(x, y, vx, vy, angle, goal) = StdIn.readLine().split(" ").map(_.toInt)
      val angleVec = if (angle == -1) {
        (context.checkPoints(goal).position - Vectorl(x, y)).norm
      } else {
        Vectorls.axisX.rotateInDegree(angle)
      }

      val absoluteGoal =
        if (context.previousPods.isEmpty) goal
        else computeGoal(context.previousPods(i).goal, context.checkPoints.size, goal)
      Pod(i, Vectorl(x, y), Vectorl(vx, vy), angleVec, absoluteGoal)
    })
    StrikeBackState(context.checkPoints, pods)
  }

  def computeGoal(previousGoal: Int, cpCount: Int, currentGoal: Int): Int = {
    if (currentGoal % cpCount < previousGoal % cpCount) {
      cpCount * (previousGoal / cpCount + 1) + currentGoal
    } else {
      cpCount * (previousGoal / cpCount) + currentGoal
    }
  }

  override def nextContext(context: StrikeBackContext,
                           state: StrikeBackState,
                           actions: Vector[StrikeBackAction]): StrikeBackContext = {
    context.copy(previousPods = state.pods)
  }


}
