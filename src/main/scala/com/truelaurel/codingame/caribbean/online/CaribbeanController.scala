package com.truelaurel.codingame.caribbean.online

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GameController

import scala.io.StdIn

/**
  * Created by hwang on 14/04/2017.
  */
object CaribbeanController extends GameController[CaribbeanContext, CaribbeanState, CaribbeanAction] {

  override def readContext: CaribbeanContext = {
    CaribbeanContext()
  }

  override def readState(turn: Int, context: CaribbeanContext): CaribbeanState = {
    val shipCount = StdIn.readInt()
    val entities = (for {
      i <- 0 until StdIn.readInt()
      line: String = StdIn.readLine()
      Array(entityId, entityType, x, y, arg1, arg2, arg3, arg4) = line.split(" ")

    } yield (entityId.toInt, entityType, x.toInt, y.toInt, arg1.toInt, arg2.toInt, arg3.toInt, arg4.toInt)).toVector

    val ships = entities.filter(_._2 == "SHIP").map(e => Ship(e._1, e._3, e._4, e._5, e._6, e._7, e._8))

    val barrels = entities.filter(_._2 == "BARREL").map(e => Barrel(e._1, e._3, e._4, e._5))

    CaribbeanState(context, ships, barrels, turn)
  }

  override def nextContext(context: CaribbeanContext, state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanContext = {
    context
  }
}
