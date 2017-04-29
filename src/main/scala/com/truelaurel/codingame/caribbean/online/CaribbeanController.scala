package com.truelaurel.codingame.caribbean.online

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.game.{GameController, GamePlayer}
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
    context
  }

  override def warmup(player: GamePlayer[CaribbeanState, CaribbeanAction]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Map(0 -> Ship(0,Offset(2,2),2,0,100,1), 5 -> Ship(5,Offset(21,14),2,0,100,0), 1 -> Ship(1,Offset(2,18),4,0,100,0), 2 -> Ship(2,Offset(8,4),5,0,100,1), 3 -> Ship(3,Offset(8,16),1,0,100,0), 4 -> Ship(4,Offset(21,6),4,0,100,1)),Map(24 -> Barrel(24,Offset(2,4),20), 37 -> Barrel(37,Offset(13,12),10), 25 -> Barrel(25,Offset(2,16),20), 20 -> Barrel(20,Offset(15,3),14), 29 -> Barrel(29,Offset(6,15),13), 28 -> Barrel(28,Offset(6,5),13), 38 -> Barrel(38,Offset(1,3),14), 21 -> Barrel(21,Offset(15,17),14), 33 -> Barrel(33,Offset(13,16),18), 32 -> Barrel(32,Offset(13,4),18), 34 -> Barrel(34,Offset(6,2),12), 17 -> Barrel(17,Offset(16,11),15), 22 -> Barrel(22,Offset(14,3),15), 27 -> Barrel(27,Offset(1,11),14), 39 -> Barrel(39,Offset(1,17),14), 35 -> Barrel(35,Offset(6,18),12), 18 -> Barrel(18,Offset(16,6),13), 16 -> Barrel(16,Offset(16,9),15), 31 -> Barrel(31,Offset(19,17),11), 26 -> Barrel(26,Offset(1,9),14), 23 -> Barrel(23,Offset(14,17),15), 36 -> Barrel(36,Offset(13,8),10), 30 -> Barrel(30,Offset(19,3),11), 19 -> Barrel(19,Offset(16,14),13)),Map(),Map(6 -> Mine(6,Offset(20,6)), 13 -> Mine(13,Offset(9,7))),1)
    (0 until 1).foreach(i => player.reactTo(state))
  }

}
