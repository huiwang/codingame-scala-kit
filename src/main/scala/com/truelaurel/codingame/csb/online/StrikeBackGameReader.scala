package com.truelaurel.codingame.csb.online

import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.csb.common.{CSBConstant, StrikeBackGameState}
import com.truelaurel.codingame.vectorial.{Vectorl, Vectorls}

import scala.io.StdIn

/**
  * Created by hwang on 07/04/2017.
  */
object StrikeBackGameReader {


  def reader(): () => StrikeBackGameState = {
    val laps = StdIn.readInt()
    val checkpointCount = StdIn.readInt()
    val checkPoints = 0.until(checkpointCount).map(i => {
      val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
      Disk(Vectorl(x, y), Vectorl(0, 0), CSBConstant.cpRadius)
    }).toVector
    readState(checkPoints)
  }

  def readState(checkPoints: Vector[Disk])(): StrikeBackGameState = {
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


}
