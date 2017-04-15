package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CabribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator

/**
  * Created by hwang on 15/04/2017.
  */
object CabribbeanOneSingleTurn {

  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(),Vector(Ship(0,13,11,2,0,80,1), Ship(1,12,4,4,1,80,0)),Vector(Barrel(11,4,12,15), Barrel(10,4,8,15), Barrel(13,10,14,15), Barrel(15,6,17,17), Barrel(14,6,3,17), Barrel(19,4,13,10), Barrel(18,4,7,10), Barrel(21,3,14,12), Barrel(20,3,6,12), Barrel(23,7,19,17), Barrel(22,7,1,17)),Vector(Ball(28,13,11,1,3)),Vector(Mine(3,10,15)),40)
    val player = CabribbeanPlayer(1, 0)
    println(player.reactTo(state))
  }
}
