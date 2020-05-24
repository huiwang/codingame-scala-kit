package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.math.geometry.Pos

sealed trait WondevAction {}

sealed case class MoveBuild(unitIndex: Int, move: Pos, build: Pos)
    extends WondevAction

sealed case class PushBuild(unitIndex: Int, build: Pos, push: Pos)
    extends WondevAction

case object AcceptDefeat extends WondevAction
