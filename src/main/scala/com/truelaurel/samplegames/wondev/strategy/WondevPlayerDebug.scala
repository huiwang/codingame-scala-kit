package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevPlayerDebug {

  def main(args: Array[String]): Unit = {

    val player = WondevPlayer(true)
    val state = WondevState(WondevContext(6,2),1,Map(Pos(2,5) -> 48, Pos(1,5) -> 48, Pos(5,0) -> 48, Pos(0,2) -> -1, Pos(0,0) -> 48, Pos(5,2) -> -1, Pos(5,1) -> 48, Pos(4,0) -> -1, Pos(3,4) -> 48, Pos(3,1) -> 48, Pos(4,1) -> -1, Pos(2,0) -> 48, Pos(0,3) -> -1, Pos(4,4) -> 48, Pos(3,0) -> 48, Pos(0,5) -> 48, Pos(1,1) -> -1, Pos(3,5) -> 48, Pos(4,5) -> 48, Pos(1,4) -> 48, Pos(0,4) -> 48, Pos(5,4) -> 48, Pos(3,2) -> 48, Pos(1,3) -> 48, Pos(2,2) -> 48, Pos(5,5) -> 48, Pos(4,2) -> 48, Pos(2,4) -> 48, Pos(0,1) -> 48, Pos(5,3) -> -1, Pos(3,3) -> 48, Pos(2,3) -> 48, Pos(1,2) -> 48, Pos(2,1) -> 48, Pos(4,3) -> 48, Pos(1,0) -> -1),List(Pos(3,0), Pos(3,4)),List(Pos(3,2), Pos(5,1)),List(MoveBuild(0,S,N), MoveBuild(0,S,NW), MoveBuild(0,S,SE), MoveBuild(0,S,SW), MoveBuild(0,S,W), MoveBuild(0,SW,E), MoveBuild(0,SW,N), MoveBuild(0,SW,NE), MoveBuild(0,SW,S), MoveBuild(0,SW,SW), MoveBuild(0,W,E), MoveBuild(0,W,S), MoveBuild(0,W,SE), MoveBuild(1,E,E), MoveBuild(1,E,N), MoveBuild(1,E,NW), MoveBuild(1,E,S), MoveBuild(1,E,SE), MoveBuild(1,E,SW), MoveBuild(1,E,W), MoveBuild(1,N,E), MoveBuild(1,N,NE), MoveBuild(1,N,NW), MoveBuild(1,N,S), MoveBuild(1,N,SE), MoveBuild(1,N,SW), MoveBuild(1,N,W), MoveBuild(1,NE,N), MoveBuild(1,NE,S), MoveBuild(1,NE,SE), MoveBuild(1,NE,SW), MoveBuild(1,NE,W), MoveBuild(1,NW,E), MoveBuild(1,NW,N), MoveBuild(1,NW,NW), MoveBuild(1,NW,S), MoveBuild(1,NW,SE), MoveBuild(1,NW,SW), MoveBuild(1,NW,W), MoveBuild(1,S,E), MoveBuild(1,S,N), MoveBuild(1,S,NE), MoveBuild(1,S,NW), MoveBuild(1,S,W), MoveBuild(1,SE,E), MoveBuild(1,SE,N), MoveBuild(1,SE,NE), MoveBuild(1,SE,NW), MoveBuild(1,SE,W), MoveBuild(1,SW,E), MoveBuild(1,SW,N), MoveBuild(1,SW,NE), MoveBuild(1,SW,NW), MoveBuild(1,SW,W), MoveBuild(1,W,E), MoveBuild(1,W,N), MoveBuild(1,W,NE), MoveBuild(1,W,NW), MoveBuild(1,W,S), MoveBuild(1,W,SE), MoveBuild(1,W,SW), MoveBuild(1,W,W)))

    player.reactTo(state)

  }
}
