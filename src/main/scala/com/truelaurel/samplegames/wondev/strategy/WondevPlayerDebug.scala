package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.math.geometry._
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevPlayerDebug {
  def main(args: Array[String]): Unit = {

    val player = WondevPlayer(true)
    val state = WondevState(WondevContext(7,2),2,Map(Pos(2,5) -> 0, Pos(1,5) -> -1, Pos(5,0) -> -1, Pos(0,2) -> -1, Pos(0,0) -> -1, Pos(5,2) -> 0, Pos(5,1) -> -1, Pos(4,0) -> -1, Pos(3,4) -> 0, Pos(6,4) -> -1, Pos(6,6) -> -1, Pos(3,1) -> 0, Pos(6,1) -> -1, Pos(4,1) -> 0, Pos(6,2) -> -1, Pos(2,0) -> -1, Pos(0,3) -> 0, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(1,6) -> -1, Pos(0,5) -> -1, Pos(3,6) -> 0, Pos(6,5) -> -1, Pos(1,1) -> -1, Pos(6,3) -> 0, Pos(3,5) -> 0, Pos(4,6) -> -1, Pos(4,5) -> 0, Pos(1,4) -> 1, Pos(2,6) -> -1, Pos(0,4) -> -1, Pos(5,4) -> 0, Pos(3,2) -> 0, Pos(1,3) -> 1, Pos(2,2) -> 0, Pos(5,5) -> -1, Pos(4,2) -> 0, Pos(2,4) -> 0, Pos(0,1) -> -1, Pos(5,3) -> 0, Pos(3,3) -> 0, Pos(2,3) -> 0, Pos(1,2) -> 0, Pos(2,1) -> 0, Pos(4,3) -> 0, Pos(6,0) -> -1, Pos(1,0) -> -1, Pos(5,6) -> -1, Pos(0,6) -> -1),List(Pos(3,4), Pos(0,3)),List(Pos(-1,-1), Pos(2,3)),List(LegalAction(Build,0,E,E), LegalAction(Build,0,E,N), LegalAction(Build,0,E,NE), LegalAction(Build,0,E,NW), LegalAction(Build,0,E,S), LegalAction(Build,0,E,SW), LegalAction(Build,0,E,W), LegalAction(Build,0,N,E), LegalAction(Build,0,N,N), LegalAction(Build,0,N,NE), LegalAction(Build,0,N,NW), LegalAction(Build,0,N,S), LegalAction(Build,0,N,SE), LegalAction(Build,0,N,SW), LegalAction(Build,0,NE,E), LegalAction(Build,0,NE,N), LegalAction(Build,0,NE,NE), LegalAction(Build,0,NE,NW), LegalAction(Build,0,NE,S), LegalAction(Build,0,NE,SE), LegalAction(Build,0,NE,SW), LegalAction(Build,0,NE,W), LegalAction(Build,0,S,E), LegalAction(Build,0,S,N), LegalAction(Build,0,S,NE), LegalAction(Build,0,S,NW), LegalAction(Build,0,S,S), LegalAction(Build,0,S,W), LegalAction(Build,0,SE,N), LegalAction(Build,0,SE,NE), LegalAction(Build,0,SE,NW), LegalAction(Build,0,SE,SW), LegalAction(Build,0,SE,W), LegalAction(Build,0,SW,E), LegalAction(Build,0,SW,N), LegalAction(Build,0,SW,NE), LegalAction(Build,0,SW,NW), LegalAction(Build,0,SW,SE), LegalAction(Build,0,W,E), LegalAction(Build,0,W,NE), LegalAction(Build,0,W,NW), LegalAction(Build,0,W,S), LegalAction(Build,0,W,SE), LegalAction(Build,0,W,W), LegalAction(Build,1,E,N), LegalAction(Build,1,E,NE), LegalAction(Build,1,E,S), LegalAction(Build,1,E,SE), LegalAction(Build,1,E,W), LegalAction(Build,1,NE,E), LegalAction(Build,1,NE,NE), LegalAction(Build,1,NE,S), LegalAction(Build,1,NE,SW), LegalAction(Build,1,SE,E), LegalAction(Build,1,SE,N), LegalAction(Build,1,SE,NW), LegalAction(Build,1,SE,SE), LegalAction(Push,0,NW,N), LegalAction(Push,0,NW,NW), LegalAction(Push,0,NW,W)))
    player.reactTo(state)

  }
}
