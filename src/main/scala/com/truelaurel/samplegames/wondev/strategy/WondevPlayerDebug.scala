package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.math.geometry._
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevPlayerDebug {
  def main(args: Array[String]): Unit = {

    val player = WondevPlayer(true)
    val state = WondevState(WondevContext(5,2,Map(Pos(0,2) -> 3, Pos(0,0) -> 1, Pos(4,0) -> 0, Pos(3,4) -> 0, Pos(3,1) -> 2, Pos(4,1) -> 2, Pos(2,0) -> 4, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 1, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 4, Pos(1,3) -> 2, Pos(2,2) -> 4, Pos(4,2) -> 1, Pos(2,4) -> 0, Pos(0,1) -> 3, Pos(3,3) -> 1, Pos(2,3) -> 1, Pos(1,2) -> 4, Pos(2,1) -> 4, Pos(4,3) -> 0, Pos(1,0) -> 3)),27,Map(Pos(0,2) -> 3, Pos(0,0) -> 2, Pos(4,0) -> 0, Pos(3,4) -> 0, Pos(3,1) -> 2, Pos(4,1) -> 2, Pos(2,0) -> 4, Pos(0,3) -> 1, Pos(4,4) -> 0, Pos(3,0) -> 1, Pos(1,1) -> 3, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(3,2) -> 4, Pos(1,3) -> 2, Pos(2,2) -> 4, Pos(4,2) -> 1, Pos(2,4) -> 0, Pos(0,1) -> 3, Pos(3,3) -> 1, Pos(2,3) -> 1, Pos(1,2) -> 4, Pos(2,1) -> 4, Pos(4,3) -> 0, Pos(1,0) -> 3),List(Pos(4,2), Pos(2,3)),List(Pos(-1,-1), Pos(-1,-1)),List(LegalAction(Build,0,N,N), LegalAction(Build,0,N,NW), LegalAction(Build,0,N,S), LegalAction(Build,0,N,W), LegalAction(Build,0,NW,E), LegalAction(Build,0,NW,N), LegalAction(Build,0,NW,NE), LegalAction(Build,0,NW,SE), LegalAction(Build,0,S,N), LegalAction(Build,0,S,S), LegalAction(Build,0,S,SW), LegalAction(Build,0,S,W), LegalAction(Build,0,SW,E), LegalAction(Build,0,SW,NE), LegalAction(Build,0,SW,S), LegalAction(Build,0,SW,SE), LegalAction(Build,0,SW,SW), LegalAction(Build,1,E,E), LegalAction(Build,1,E,S), LegalAction(Build,1,E,SE), LegalAction(Build,1,E,SW), LegalAction(Build,1,E,W), LegalAction(Build,1,S,E), LegalAction(Build,1,S,N), LegalAction(Build,1,S,NE), LegalAction(Build,1,S,NW), LegalAction(Build,1,S,W), LegalAction(Build,1,SE,E), LegalAction(Build,1,SE,N), LegalAction(Build,1,SE,NE), LegalAction(Build,1,SE,NW), LegalAction(Build,1,SE,W), LegalAction(Build,1,SW,E), LegalAction(Build,1,SW,N), LegalAction(Build,1,SW,NE), LegalAction(Build,1,SW,NW), LegalAction(Build,1,SW,W), LegalAction(Build,1,W,E), LegalAction(Build,1,W,NW), LegalAction(Build,1,W,S), LegalAction(Build,1,W,SE), LegalAction(Build,1,W,SW), LegalAction(Build,1,W,W)))

    player.reactTo(state)

  }
}
