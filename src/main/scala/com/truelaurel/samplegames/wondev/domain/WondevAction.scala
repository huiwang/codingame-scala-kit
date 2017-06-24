package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos


sealed trait WondevAction

sealed case class MoveBuild(unitIndex: Int, moveDir: Pos, buildDir: Pos) extends WondevAction {
  override def toString: String = s"MOVE&BUILD $unitIndex $moveDir $buildDir"
}

sealed case class MovePush(unitIndex: Int, moveDir: Pos, pushDir: Pos) extends WondevAction {
  override def toString: String = s"PUSH&BUILD $unitIndex $moveDir $pushDir"
}
