package com.truelaurel.codingame.caribbean.online

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.engine.{GameController, GamePlayer}
import com.truelaurel.codingame.hexagons.Offset

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

    val ships = entities.filter(_._2 == "SHIP").map(e => Ship(e._1, Offset(e._3, e._4), e._5, e._6, e._7, e._8))

    val barrels = entities.filter(_._2 == "BARREL").map(e => Barrel(e._1, Offset(e._3, e._4), e._5))

    val balls = entities.filter(_._2 == "CANNONBALL").map(e => Ball(e._1, Offset(e._3, e._4), e._5, e._6))

    val mines = entities.filter(_._2 == "MINE").map(e => Mine(e._1, Offset(e._3, e._4)))

    CaribbeanState(context, ships.sortBy(_.id), barrels, balls, mines, turn)
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

  override def warmup(player: GamePlayer[CaribbeanState, CaribbeanAction]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(), Map()), Vector(Ship(0, Offset(10, 1), 5, 0, 100, 1), Ship(1, Offset(10, 19), 1, 0, 100, 0)), Vector(Barrel(12, Offset(20, 19), 11), Barrel(11, Offset(20, 1), 11), Barrel(14, Offset(15, 19), 15), Barrel(13, Offset(15, 1), 15), Barrel(16, Offset(4, 16), 11), Barrel(15, Offset(4, 4), 11), Barrel(18, Offset(18, 13), 18), Barrel(17, Offset(18, 7), 18), Barrel(20, Offset(15, 17), 11), Barrel(19, Offset(15, 3), 11), Barrel(22, Offset(13, 17), 18), Barrel(21, Offset(13, 3), 18), Barrel(24, Offset(8, 17), 20), Barrel(23, Offset(8, 3), 20), Barrel(26, Offset(3, 12), 16), Barrel(25, Offset(3, 8), 16), Barrel(28, Offset(4, 15), 13), Barrel(27, Offset(4, 5), 13), Barrel(30, Offset(14, 12), 15), Barrel(29, Offset(14, 8), 15)), Vector(), Vector(Mine(9, Offset(12, 6))), 1)
    (0 until 10).foreach(i => player.reactTo(state))
  }

}
