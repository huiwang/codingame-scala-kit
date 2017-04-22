package com.truelaurel.codingame.caribbean.offline

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.hexagons.Offset
import com.truelaurel.codingame.time.CountStopper

/**
  * Created by hwang on 15/04/2017.
  */
object CaribbeanPlayerDebug {

  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map(1 -> 18, 3 -> 19)),Map(1 -> Ship(1,Offset(7,9),0,2,89,1), 3 -> Ship(3,Offset(12,4),1,1,93,1), 0 -> Ship(0,Offset(12,13),2,2,97,0), 2 -> Ship(2,Offset(20,13),1,2,98,0)),Map(14 -> Barrel(14,Offset(16,5),20), 16 -> Barrel(16,Offset(12,2),14), 27 -> Barrel(27,Offset(10,9),14), 31 -> Barrel(31,Offset(17,7),12)),Map(38 -> Ball(38,Offset(14,17),3,0), 39 -> Ball(39,Offset(10,9),0,3)),Map(5 -> Mine(5,Offset(4,12)), 4 -> Mine(4,Offset(4,8))),24)

    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
