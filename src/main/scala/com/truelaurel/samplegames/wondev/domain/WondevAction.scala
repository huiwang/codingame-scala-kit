package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Direction

sealed trait WondevAction

sealed case class MoveBuild(unitIndex: Int, moveDir: Direction, buildDir: Direction) extends WondevAction {
  override def toString: String = s"MOVE&BUILD $unitIndex $moveDir $buildDir"
}

sealed case class MovePush(unitIndex: Int, moveDir: Direction, pushDir: Direction) extends WondevAction {
  override def toString: String = s"PUSH&BUILD $unitIndex $moveDir $pushDir"
}
