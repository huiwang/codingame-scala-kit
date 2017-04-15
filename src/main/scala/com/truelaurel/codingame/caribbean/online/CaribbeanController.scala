package com.truelaurel.codingame.caribbean.online

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.GameController
import com.truelaurel.codingame.hexagons.Offset

import scala.io.StdIn

/**
  * Created by hwang on 14/04/2017.
  */
object CaribbeanController extends GameController[CaribbeanContext, CaribbeanState, CaribbeanAction] {

  override def readContext: CaribbeanContext = {
    CaribbeanContext(Map.empty, Map.empty)
  }

  override def readState(turn: Int, context: CaribbeanContext): CaribbeanState = {
    val shipCount = StdIn.readInt()
    val entities = (for {
      i <- 0 until StdIn.readInt()
      line: String = StdIn.readLine()
      Array(entityId, entityType, x, y, arg1, arg2, arg3, arg4) = line.split(" ")

    } yield (entityId.toInt, entityType, x.toInt, y.toInt, arg1.toInt, arg2.toInt, arg3.toInt, arg4.toInt)).toVector

    val ships = entities.filter(_._2 == "SHIP").map(e => Ship(e._1, Offset(e._3, e._4), e._5, e._6, e._7, e._8))

    val barrels = entities.filter(_._2 == "BARREL").map(e => Barrel(e._1, Offset(e._3, e._4), e._5))

    val balls = entities.filter(_._2 == "CANNONBALL").map(e => Ball(e._1, Offset(e._3, e._4), e._5, e._6))

    val mines = entities.filter(_._2 == "MINE").map(e => Mine(e._1, Offset(e._3, e._4)))

    CaribbeanState(context, ships, barrels, balls, mines, turn)
  }

  override def nextContext(context: CaribbeanContext, state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanContext = {
    val mines = actions.flatMap {
      case MineAction(shipId) => Some(shipId -> state.turn)
      case _ => None
    }.toMap

    val fires = actions.flatMap {
      case Fire(shipId, _) => Some(shipId -> state.turn)
      case _ => None
    }.toMap

    CaribbeanContext(context.lastMine ++ mines, context.lastFire ++ fires)
  }
}
