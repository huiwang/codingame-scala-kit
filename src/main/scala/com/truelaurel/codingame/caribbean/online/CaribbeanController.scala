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

    val entities = for {
      i <- 0 until StdIn.readInt()
      line: String = StdIn.readLine()
    } yield line

    var ships: Map[Int, Ship] = Map.empty

    var barrels: Map[Int,Barrel] = Map.empty

    var balls: Map[Int,Ball] = Map.empty

    var mines: Map[Int,Mine] = Map.empty

    for {
      line <- entities
      Array(entityId, entityType, x, y, arg1, arg2, arg3, arg4) = line.split(" ")
    } {
      val id = entityId.toInt
      val offset = Offset(x.toInt, y.toInt)
      val a1 = arg1.toInt
      val a2 = arg2.toInt
      entityType match {
        case "SHIP" => ships = ships.updated(id, Ship(id, offset, a1, a2, arg3.toInt, arg4.toInt))
        case "BARREL" => barrels = barrels.updated(id, Barrel(id, offset, a1))
        case "CANNONBALL" => balls = balls.updated(id, Ball(id, offset, a1, a2))
        case "MINE" => mines = mines.updated(id, Mine(id, offset))
      }
    }

    CaribbeanState(context, ships, barrels, balls, mines, turn)
  }


  override def nextContext(context: CaribbeanContext, state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanContext = {
    val fires = actions.flatMap {
      case Fire(shipId, _) => Some(shipId -> state.turn)
      case _ => None
    }.toMap

    CaribbeanContext(context.mines, context.lastFire ++ fires)
  }

  override def warmup(player: GamePlayer[CaribbeanState, CaribbeanAction]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map(0 -> 31)),Map(0 -> Ship(0,Offset(16,14),2,0,90,1), 1 -> Ship(1,Offset(6,17),3,2,100,0)),Map(10 -> Barrel(10,Offset(14,6),18), 14 -> Barrel(14,Offset(4,7),19), 29 -> Barrel(29,Offset(15,15),15), 28 -> Barrel(28,Offset(15,5),15), 21 -> Barrel(21,Offset(16,2),10), 33 -> Barrel(33,Offset(20,5),13), 34 -> Barrel(34,Offset(20,15),13), 12 -> Barrel(12,Offset(6,3),11), 31 -> Barrel(31,Offset(20,3),12), 23 -> Barrel(23,Offset(7,1),11), 19 -> Barrel(19,Offset(3,6),13), 15 -> Barrel(15,Offset(4,13),19)),Map(),Map(3 -> Mine(3,Offset(13,17)), 44 -> Mine(44,Offset(13,19))),33)
    (0 until 1).foreach(i => player.reactTo(state))
  }

}
