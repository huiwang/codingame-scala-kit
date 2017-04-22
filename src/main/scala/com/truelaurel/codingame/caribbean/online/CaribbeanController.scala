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
        case "SHIP" => {
          System.err.println("shipId " + id)
          ships = ships.updated(id, Ship(id, offset, a1, a2, arg3.toInt, arg4.toInt))
        }
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

    CaribbeanContext(context.lastMine, context.lastFire ++ fires)
  }

  override def warmup(player: GamePlayer[CaribbeanState, CaribbeanAction]): Unit = {
    //val state = CaribbeanState(CaribbeanContext(Map(), Map()), Vector(Ship(0, Offset(2, 6), 1, 0, 100, 0), Ship(1, Offset(2, 14), 5, 0, 100, 1), Ship(2, Offset(8, 3), 4, 0, 100, 0), Ship(3, Offset(8, 17), 2, 0, 100, 1), Ship(4, Offset(16, 2), 5, 0, 100, 0), Ship(5, Offset(16, 18), 1, 0, 100, 1)), Vector(Barrel(12, Offset(18, 10), 10), Barrel(14, Offset(12, 12), 15), Barrel(13, Offset(12, 8), 15), Barrel(16, Offset(11, 14), 17), Barrel(15, Offset(11, 6), 17), Barrel(18, Offset(3, 17), 11), Barrel(17, Offset(3, 3), 11), Barrel(20, Offset(1, 14), 11), Barrel(19, Offset(1, 6), 11), Barrel(22, Offset(13, 18), 15), Barrel(21, Offset(13, 2), 15), Barrel(24, Offset(9, 12), 15), Barrel(23, Offset(9, 8), 15)), Vector(), Vector(Mine(7, Offset(2, 16)), Mine(9, Offset(20, 16))), 1)
    //(0 until 1).foreach(i => player.reactTo(state))
  }

}
