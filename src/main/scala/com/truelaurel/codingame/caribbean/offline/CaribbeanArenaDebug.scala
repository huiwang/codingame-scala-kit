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
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Vector(Ship(0,Offset(1,0),3,2,90,1), Ship(1,Offset(13,1),3,2,87,0)),Vector(Barrel(9,Offset(15,19),12), Barrel(11,Offset(8,15),17), Barrel(10,Offset(8,5),17), Barrel(12,Offset(17,5),17), Barrel(15,Offset(20,18),13), Barrel(14,Offset(20,2),13), Barrel(17,Offset(5,14),17), Barrel(16,Offset(5,6),17)),Vector(),Vector(Mine(6,Offset(3,3))),16)
    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = BestCabribbeanPlayer(0, 1)
    val after = GameSimulator.singleTurn(state, CaribbeanArena, Vector(myPlayer, otherPlayer))
    println(after)
  }
}
