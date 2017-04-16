package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.best.BestCabribbeanPlayer
import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.hexagons.Offset

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanArenaDebug {

  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(19,1),4,0,100,1), Ship(1,Offset(19,19),2,0,100,0)),Vector(Barrel(11,Offset(9,18),15), Barrel(10,Offset(9,2),15), Barrel(13,Offset(2,12),18), Barrel(12,Offset(2,8),18), Barrel(15,Offset(6,14),19), Barrel(14,Offset(6,6),19), Barrel(17,Offset(6,16),11), Barrel(16,Offset(6,4),11), Barrel(19,Offset(15,17),17), Barrel(18,Offset(15,3),17), Barrel(21,Offset(8,12),20), Barrel(20,Offset(8,8),20), Barrel(23,Offset(7,13),15), Barrel(22,Offset(7,7),15), Barrel(25,Offset(2,18),15), Barrel(24,Offset(2,2),15), Barrel(27,Offset(4,17),10), Barrel(26,Offset(4,3),10), Barrel(29,Offset(6,13),10), Barrel(28,Offset(6,7),10), Barrel(30,Offset(13,10),10)),Vector(),Vector(Mine(8,Offset(15,1))),1)
    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = BestCabribbeanPlayer(0, 1)
    val after = GameSimulator.singleTurn(state, CaribbeanArena, Vector(myPlayer, otherPlayer))
    println(after)
  }
}
