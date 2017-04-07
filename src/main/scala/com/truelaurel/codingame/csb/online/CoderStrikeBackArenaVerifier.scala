package com.truelaurel.codingame.csb.online

import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.csb.common.{CSBConstant, PodAction, StrikeBackGameState}
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.offline.StrikeBackArena
import com.truelaurel.codingame.engine.{ArenaVerifier, GameArena, GamePlayer}
import com.truelaurel.codingame.vectorial.{Vectorl, Vectorls}

import scala.io.StdIn

/**
  * Created by hwang on 07/04/2017.
  */
class CoderStrikeBackArenaVerifier() extends ArenaVerifier[StrikeBackGameState, PodAction] {

  val me = StrikeBackPlayer(Vector(0, 1))
  var checkPoints: Vector[Disk] = _

  override def preGame(): Unit = {
    val laps = StdIn.readInt()
    val checkpointCount = StdIn.readInt()
    checkPoints = 0.until(checkpointCount).map(i => {
      val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
      Disk(Vectorl(x, y), Vectorl(0, 0), CSBConstant.cpRadius)
    }).toVector
  }

  override def readState(): StrikeBackGameState = {
    val pods = CSBConstant.podIndices.map(i => {
      val Array(x, y, vx, vy, angle, nextcheckpointid) = StdIn.readLine().split(" ").map(_.toInt)
      (Disk(Vectorl(x, y), Vectorl(vx, vy), 400), angle, nextcheckpointid)
    })

    val disks: Vector[Disk] = pods.map(_._1)
    val angles: Vector[Vectorl] = pods.map(_._2)
      .map(angle => if (angle == -1) Vectorls.origin else Vectorls.axisX.rotateInDegree(angle))
    val nextCheck: Vector[Int] = pods.map(_._3)
    StrikeBackGameState(checkPoints, disks, angles, nextCheck)
  }

  override def mePlayer(): GamePlayer[StrikeBackGameState, PodAction] = StrikeBackPlayer(Vector(0, 1))

  override def otherPlayer(): GamePlayer[StrikeBackGameState, PodAction] = StrikeBackPlayer(Vector(2, 3))

  override def arena(): GameArena[StrikeBackGameState, PodAction] = StrikeBackArena
}
