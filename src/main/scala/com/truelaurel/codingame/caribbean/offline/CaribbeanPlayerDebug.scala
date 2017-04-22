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
    val state = CaribbeanState(CaribbeanContext(Map(),Map(0 -> 31)),Map(0 -> Ship(0,Offset(16,14),2,0,90,1), 1 -> Ship(1,Offset(6,17),3,2,100,0)),Map(10 -> Barrel(10,Offset(14,6),18), 14 -> Barrel(14,Offset(4,7),19), 29 -> Barrel(29,Offset(15,15),15), 28 -> Barrel(28,Offset(15,5),15), 21 -> Barrel(21,Offset(16,2),10), 33 -> Barrel(33,Offset(20,5),13), 34 -> Barrel(34,Offset(20,15),13), 12 -> Barrel(12,Offset(6,3),11), 31 -> Barrel(31,Offset(20,3),12), 23 -> Barrel(23,Offset(7,1),11), 19 -> Barrel(19,Offset(3,6),13), 15 -> Barrel(15,Offset(4,13),19)),Map(),Map(3 -> Mine(3,Offset(13,17)), 44 -> Mine(44,Offset(13,19))),33)

    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
