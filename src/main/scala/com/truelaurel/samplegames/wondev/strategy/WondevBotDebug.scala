package com.truelaurel.samplegames.wondev.strategy

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WarFogCleaner
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, PushBuild, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevIO

/**
  * Created by hwang on 24/06/2017.
  */
object WondevBotDebug {

  def main(args: Array[String]): Unit = {

    val player = MinimaxPlayer
    val observed = WondevState(6,Map(Pos(2,5) -> 0, Pos(1,5) -> 0, Pos(5,0) -> 0, Pos(0,2) -> 0, Pos(0,0) -> 0, Pos(5,2) -> 0, Pos(5,1) -> 0, Pos(4,0) -> -1, Pos(3,4) -> 2, Pos(3,1) -> -1, Pos(4,1) -> 0, Pos(2,0) -> 0, Pos(0,3) -> 0, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(0,5) -> 0, Pos(1,1) -> 1, Pos(3,5) -> 0, Pos(4,5) -> 0, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(5,4) -> 0, Pos(3,2) -> 0, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(5,5) -> 0, Pos(4,2) -> 0, Pos(2,4) -> 0, Pos(0,1) -> 1, Pos(5,3) -> 0, Pos(3,3) -> 0, Pos(2,3) -> 0, Pos(1,2) -> 0, Pos(2,1) -> -1, Pos(4,3) -> 0, Pos(1,0) -> -1),List(Pos(0,0), Pos(3,3), Pos(3,2), Pos(1,1)),List(MoveBuild(0,Pos(0,1),Pos(0,0)), MoveBuild(0,Pos(0,1),Pos(0,2)), MoveBuild(0,Pos(0,1),Pos(1,2)), MoveBuild(1,Pos(4,3),Pos(5,3)), MoveBuild(1,Pos(4,3),Pos(4,2)), MoveBuild(1,Pos(4,3),Pos(5,2)), MoveBuild(1,Pos(4,3),Pos(4,4)), MoveBuild(1,Pos(4,3),Pos(5,4)), MoveBuild(1,Pos(4,3),Pos(3,4)), MoveBuild(1,Pos(4,3),Pos(3,3)), MoveBuild(1,Pos(4,2),Pos(5,2)), MoveBuild(1,Pos(4,2),Pos(4,1)), MoveBuild(1,Pos(4,2),Pos(5,1)), MoveBuild(1,Pos(4,2),Pos(4,3)), MoveBuild(1,Pos(4,2),Pos(5,3)), MoveBuild(1,Pos(4,2),Pos(3,3)), MoveBuild(1,Pos(2,2),Pos(2,3)), MoveBuild(1,Pos(2,2),Pos(3,3)), MoveBuild(1,Pos(2,2),Pos(1,3)), MoveBuild(1,Pos(2,2),Pos(1,2)), MoveBuild(1,Pos(4,4),Pos(5,4)), MoveBuild(1,Pos(4,4),Pos(4,3)), MoveBuild(1,Pos(4,4),Pos(5,3)), MoveBuild(1,Pos(4,4),Pos(3,3)), MoveBuild(1,Pos(4,4),Pos(4,5)), MoveBuild(1,Pos(4,4),Pos(5,5)), MoveBuild(1,Pos(4,4),Pos(3,5)), MoveBuild(1,Pos(4,4),Pos(3,4)), MoveBuild(1,Pos(2,4),Pos(3,4)), MoveBuild(1,Pos(2,4),Pos(2,3)), MoveBuild(1,Pos(2,4),Pos(3,3)), MoveBuild(1,Pos(2,4),Pos(1,3)), MoveBuild(1,Pos(2,4),Pos(2,5)), MoveBuild(1,Pos(2,4),Pos(3,5)), MoveBuild(1,Pos(2,4),Pos(1,5)), MoveBuild(1,Pos(2,4),Pos(1,4)), MoveBuild(1,Pos(2,3),Pos(3,3)), MoveBuild(1,Pos(2,3),Pos(2,2)), MoveBuild(1,Pos(2,3),Pos(1,2)), MoveBuild(1,Pos(2,3),Pos(2,4)), MoveBuild(1,Pos(2,3),Pos(3,4)), MoveBuild(1,Pos(2,3),Pos(1,4)), MoveBuild(1,Pos(2,3),Pos(1,3)), PushBuild(0,Pos(1,1),Pos(1,2)), PushBuild(0,Pos(1,1),Pos(2,2)), PushBuild(1,Pos(3,2),Pos(4,1))),true)



    val previousAction = MoveBuild(1,Pos(3,3),Pos(3,4))


    val previousOppoScope = Set(Set(Pos(5,5), Pos(2,2)), Set(Pos(3,0), Pos(2,2)), Set(Pos(2,2), Pos(1,2)), Set(Pos(4,2), Pos(0,2)), Set(Pos(4,2), Pos(2,2)), Set(Pos(5,4), Pos(2,2)), Set(Pos(5,4), Pos(0,2)), Set(Pos(2,2), Pos(4,3)))



    val previousState = WondevState(6,Map(Pos(2,5) -> 0, Pos(1,5) -> 0, Pos(5,0) -> 0, Pos(0,2) -> 0, Pos(0,0) -> 0, Pos(5,2) -> 0, Pos(5,1) -> 0, Pos(4,0) -> -1, Pos(3,4) -> 1, Pos(3,1) -> -1, Pos(4,1) -> 0, Pos(2,0) -> 0, Pos(0,3) -> 0, Pos(4,4) -> 0, Pos(3,0) -> 0, Pos(0,5) -> 0, Pos(1,1) -> 1, Pos(3,5) -> 0, Pos(4,5) -> 0, Pos(1,4) -> 0, Pos(0,4) -> 0, Pos(5,4) -> 0, Pos(3,2) -> 0, Pos(1,3) -> 0, Pos(2,2) -> 0, Pos(5,5) -> 0, Pos(4,2) -> 0, Pos(2,4) -> 0, Pos(0,1) -> 0, Pos(5,3) -> 0, Pos(3,3) -> 0, Pos(2,3) -> 0, Pos(1,2) -> 0, Pos(2,1) -> -1, Pos(4,3) -> 0, Pos(1,0) -> -1),List(Pos(0,0), Pos(2,4), Pos(-1,-1), Pos(-1,-1)),List(MoveBuild(0,Pos(0,1),Pos(1,1)), MoveBuild(0,Pos(0,1),Pos(0,0)), MoveBuild(0,Pos(0,1),Pos(0,2)), MoveBuild(0,Pos(0,1),Pos(1,2)), MoveBuild(0,Pos(1,1),Pos(2,0)), MoveBuild(0,Pos(1,1),Pos(0,0)), MoveBuild(0,Pos(1,1),Pos(1,2)), MoveBuild(0,Pos(1,1),Pos(2,2)), MoveBuild(0,Pos(1,1),Pos(0,2)), MoveBuild(0,Pos(1,1),Pos(0,1)), MoveBuild(1,Pos(3,4),Pos(4,4)), MoveBuild(1,Pos(3,4),Pos(3,3)), MoveBuild(1,Pos(3,4),Pos(4,3)), MoveBuild(1,Pos(3,4),Pos(2,3)), MoveBuild(1,Pos(3,4),Pos(3,5)), MoveBuild(1,Pos(3,4),Pos(4,5)), MoveBuild(1,Pos(3,4),Pos(2,5)), MoveBuild(1,Pos(3,4),Pos(2,4)), MoveBuild(1,Pos(2,3),Pos(3,3)), MoveBuild(1,Pos(2,3),Pos(2,2)), MoveBuild(1,Pos(2,3),Pos(3,2)), MoveBuild(1,Pos(2,3),Pos(1,2)), MoveBuild(1,Pos(2,3),Pos(2,4)), MoveBuild(1,Pos(2,3),Pos(3,4)), MoveBuild(1,Pos(2,3),Pos(1,4)), MoveBuild(1,Pos(2,3),Pos(1,3)), MoveBuild(1,Pos(3,3),Pos(4,3)), MoveBuild(1,Pos(3,3),Pos(3,2)), MoveBuild(1,Pos(3,3),Pos(4,2)), MoveBuild(1,Pos(3,3),Pos(2,2)), MoveBuild(1,Pos(3,3),Pos(3,4)), MoveBuild(1,Pos(3,3),Pos(4,4)), MoveBuild(1,Pos(3,3),Pos(2,4)), MoveBuild(1,Pos(3,3),Pos(2,3)), MoveBuild(1,Pos(1,3),Pos(2,3)), MoveBuild(1,Pos(1,3),Pos(1,2)), MoveBuild(1,Pos(1,3),Pos(2,2)), MoveBuild(1,Pos(1,3),Pos(0,2)), MoveBuild(1,Pos(1,3),Pos(1,4)), MoveBuild(1,Pos(1,3),Pos(2,4)), MoveBuild(1,Pos(1,3),Pos(0,4)), MoveBuild(1,Pos(1,3),Pos(0,3)), MoveBuild(1,Pos(2,5),Pos(3,5)), MoveBuild(1,Pos(2,5),Pos(2,4)), MoveBuild(1,Pos(2,5),Pos(3,4)), MoveBuild(1,Pos(2,5),Pos(1,4)), MoveBuild(1,Pos(2,5),Pos(1,5)), MoveBuild(1,Pos(3,5),Pos(4,5)), MoveBuild(1,Pos(3,5),Pos(3,4)), MoveBuild(1,Pos(3,5),Pos(4,4)), MoveBuild(1,Pos(3,5),Pos(2,4)), MoveBuild(1,Pos(3,5),Pos(2,5)), MoveBuild(1,Pos(1,5),Pos(2,5)), MoveBuild(1,Pos(1,5),Pos(1,4)), MoveBuild(1,Pos(1,5),Pos(2,4)), MoveBuild(1,Pos(1,5),Pos(0,4)), MoveBuild(1,Pos(1,5),Pos(0,5)), MoveBuild(1,Pos(1,4),Pos(2,4)), MoveBuild(1,Pos(1,4),Pos(1,3)), MoveBuild(1,Pos(1,4),Pos(2,3)), MoveBuild(1,Pos(1,4),Pos(0,3)), MoveBuild(1,Pos(1,4),Pos(1,5)), MoveBuild(1,Pos(1,4),Pos(2,5)), MoveBuild(1,Pos(1,4),Pos(0,5)), MoveBuild(1,Pos(1,4),Pos(0,4))),true)

    val updated = WarFogCleaner.restrictOppoScope(observed, previousState, previousAction, previousOppoScope)

    val removed = WarFogCleaner.removeFog(observed, updated)

    val action = MinimaxPlayer.react(removed)

    WondevIO.writeAction(removed, action)
  }
}
