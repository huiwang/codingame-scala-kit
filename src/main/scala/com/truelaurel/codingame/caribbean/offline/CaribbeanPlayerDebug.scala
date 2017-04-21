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
    val state = CaribbeanState(CaribbeanContext(Map(),Map(0 -> 54, 2 -> 34, 4 -> 48)),Vector(Ship(0,Offset(20,4),0,0,73,1), Ship(4,Offset(21,3),2,0,25,1), Ship(1,Offset(10,12),2,2,89,0), Ship(3,Offset(11,15),4,2,64,0), Ship(5,Offset(15,17),3,2,58,0)),Vector(),Vector(Ball(68,Offset(22,3),0,1)),Vector(Mine(8,Offset(20,5)), Mine(10,Offset(21,1))),56)


    val player = CaribbeanPlayer(1, 0)
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
