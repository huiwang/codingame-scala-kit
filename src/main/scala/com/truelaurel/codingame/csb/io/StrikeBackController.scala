package com.truelaurel.codingame.csb.io

import com.truelaurel.codingame.csb.model._
import com.truelaurel.codingame.engine.GameController
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
    StrikeBackContext(checkPoints)
  }

  override def readState(turn: Int, context: StrikeBackContext): StrikeBackState = {
    val pods = StrikeBackContext.podIndices.map(i => {
      val Array(x, y, vx, vy, angle, goal) = StdIn.readLine().split(" ").map(_.toInt)
      val angleVec = if (angle == -1) {
        (context.checkPoints(goal).position - Vectorl(x, y)).norm
      } else {
        Vectorls.axisX.rotateInDegree(angle)
      }
      Pod(i, Vectorl(x, y), Vectorl(vx, vy), angleVec, goal)
    })
    StrikeBackState(context.checkPoints, pods)
  }

  override def nextContext(context: StrikeBackContext,
                           state: StrikeBackState,
                           actions: Vector[StrikeBackAction]): StrikeBackContext = context


}
