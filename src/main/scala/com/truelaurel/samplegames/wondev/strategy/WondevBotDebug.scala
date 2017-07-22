package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.FastWarFogAnalysis
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, WondevState}

/**
  * Created by hwang on 24/06/2017.
  */
object WondevBotDebug {

  def main(args: Array[String]): Unit = {

    val bot = MinimaxPlayer
    val state = WondevState(7,Map(Pos(2,5) -> 0, Pos(1,5) -> -1, Pos(5,0) -> -1, Pos(0,2) -> -1, Pos(0,0) -> -1, Pos(5,2) -> 0, Pos(5,1) -> -1, Pos(4,0) -> -1, Pos(3,4) -> 0, Pos(6,4) -> -1, Pos(6,6) -> -1, Pos(3,1) -> 0, Pos(6,1) -> -1, Pos(4,1) -> 0, Pos(6,2) -> -1, Pos(2,0) -> -1, Pos(0,3) -> 0, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(1,6) -> -1, Pos(0,5) -> -1, Pos(3,6) -> 0, Pos(6,5) -> -1, Pos(1,1) -> -1, Pos(6,3) -> 0, Pos(3,5) -> 0, Pos(4,6) -> -1, Pos(4,5) -> 0, Pos(1,4) -> 0, Pos(2,6) -> -1, Pos(0,4) -> -1, Pos(5,4) -> 0, Pos(3,2) -> 0, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(5,5) -> -1, Pos(4,2) -> 0, Pos(2,4) -> 0, Pos(0,1) -> -1, Pos(5,3) -> 0, Pos(3,3) -> 0, Pos(2,3) -> 0, Pos(1,2) -> 0, Pos(2,1) -> 0, Pos(4,3) -> 0, Pos(6,0) -> -1, Pos(1,0) -> -1, Pos(5,6) -> -1, Pos(0,6) -> -1),List(Pos(4,3), Pos(4,2), Pos(-1,-1), Pos(-1,-1)),List(MoveBuild(0,Pos(5,3),Pos(6,3)), MoveBuild(0,Pos(5,3),Pos(5,2)), MoveBuild(0,Pos(5,3),Pos(5,4)), MoveBuild(0,Pos(5,3),Pos(4,4)), MoveBuild(0,Pos(5,3),Pos(4,3)), MoveBuild(0,Pos(5,2),Pos(4,1)), MoveBuild(0,Pos(5,2),Pos(5,3)), MoveBuild(0,Pos(5,2),Pos(6,3)), MoveBuild(0,Pos(5,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(3,1)), MoveBuild(0,Pos(3,2),Pos(4,1)), MoveBuild(0,Pos(3,2),Pos(2,1)), MoveBuild(0,Pos(3,2),Pos(3,3)), MoveBuild(0,Pos(3,2),Pos(4,3)), MoveBuild(0,Pos(3,2),Pos(2,3)), MoveBuild(0,Pos(3,2),Pos(2,2)), MoveBuild(0,Pos(4,4),Pos(5,4)), MoveBuild(0,Pos(4,4),Pos(4,3)), MoveBuild(0,Pos(4,4),Pos(5,3)), MoveBuild(0,Pos(4,4),Pos(3,3)), MoveBuild(0,Pos(4,4),Pos(4,5)), MoveBuild(0,Pos(4,4),Pos(3,5)), MoveBuild(0,Pos(4,4),Pos(3,4)), MoveBuild(0,Pos(5,4),Pos(5,3)), MoveBuild(0,Pos(5,4),Pos(6,3)), MoveBuild(0,Pos(5,4),Pos(4,3)), MoveBuild(0,Pos(5,4),Pos(4,5)), MoveBuild(0,Pos(5,4),Pos(4,4)), MoveBuild(0,Pos(3,4),Pos(4,4)), MoveBuild(0,Pos(3,4),Pos(3,3)), MoveBuild(0,Pos(3,4),Pos(4,3)), MoveBuild(0,Pos(3,4),Pos(2,3)), MoveBuild(0,Pos(3,4),Pos(3,5)), MoveBuild(0,Pos(3,4),Pos(4,5)), MoveBuild(0,Pos(3,4),Pos(2,5)), MoveBuild(0,Pos(3,4),Pos(2,4)), MoveBuild(0,Pos(3,3),Pos(4,3)), MoveBuild(0,Pos(3,3),Pos(3,2)), MoveBuild(0,Pos(3,3),Pos(2,2)), MoveBuild(0,Pos(3,3),Pos(3,4)), MoveBuild(0,Pos(3,3),Pos(4,4)), MoveBuild(0,Pos(3,3),Pos(2,4)), MoveBuild(0,Pos(3,3),Pos(2,3)), MoveBuild(1,Pos(5,2),Pos(4,1)), MoveBuild(1,Pos(5,2),Pos(5,3)), MoveBuild(1,Pos(5,2),Pos(6,3)), MoveBuild(1,Pos(5,2),Pos(4,2)), MoveBuild(1,Pos(4,1),Pos(3,0)), MoveBuild(1,Pos(4,1),Pos(4,2)), MoveBuild(1,Pos(4,1),Pos(5,2)), MoveBuild(1,Pos(4,1),Pos(3,2)), MoveBuild(1,Pos(4,1),Pos(3,1)), MoveBuild(1,Pos(3,1),Pos(4,1)), MoveBuild(1,Pos(3,1),Pos(3,0)), MoveBuild(1,Pos(3,1),Pos(3,2)), MoveBuild(1,Pos(3,1),Pos(4,2)), MoveBuild(1,Pos(3,1),Pos(2,2)), MoveBuild(1,Pos(3,1),Pos(2,1)), MoveBuild(1,Pos(5,3),Pos(6,3)), MoveBuild(1,Pos(5,3),Pos(5,2)), MoveBuild(1,Pos(5,3),Pos(4,2)), MoveBuild(1,Pos(5,3),Pos(5,4)), MoveBuild(1,Pos(5,3),Pos(4,4)), MoveBuild(1,Pos(3,3),Pos(3,2)), MoveBuild(1,Pos(3,3),Pos(4,2)), MoveBuild(1,Pos(3,3),Pos(2,2)), MoveBuild(1,Pos(3,3),Pos(3,4)), MoveBuild(1,Pos(3,3),Pos(4,4)), MoveBuild(1,Pos(3,3),Pos(2,4)), MoveBuild(1,Pos(3,3),Pos(2,3)), MoveBuild(1,Pos(3,2),Pos(4,2)), MoveBuild(1,Pos(3,2),Pos(3,1)), MoveBuild(1,Pos(3,2),Pos(4,1)), MoveBuild(1,Pos(3,2),Pos(2,1)), MoveBuild(1,Pos(3,2),Pos(3,3)), MoveBuild(1,Pos(3,2),Pos(2,3)), MoveBuild(1,Pos(3,2),Pos(2,2))),true)

    println(FastWarFogAnalysis.restrictOppoScope(state, null, null, null))

  }
}
