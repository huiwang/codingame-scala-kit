package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.common.StrikeBackGameState
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.vectorial.Vectorl
import org.scalatest.FlatSpec

/**
  * Created by hwang on 04/04/2017.
  */
class StrikeBackArenaTest extends FlatSpec {

  behavior of "Strike back arena"
  it should "simulate one turn" in {
    val state = StrikeBackGameState(Vector(Disk(Vectorl(7514, 6910),Vectorl(0, 0),200.0,1.0), Disk(Vectorl(5975, 5364),Vectorl(0, 0),200.0,1.0), Disk(Vectorl(11323, 2820),Vectorl(0, 0),200.0,1.0)),Vector(Disk(Vectorl(12059, 6507),Vectorl(-375, 167),400.0,1.0), Disk(Vectorl(11393, 6026),Vectorl(-245, 309),400.0,1.0), Disk(Vectorl(12500, 4454),Vectorl(-204, 281),400.0,1.0), Disk(Vectorl(12313, 1033),Vectorl(-229, 164),400.0,1.0)),Vector(Vectorl(-9, 1), Vectorl(-9, 2), Vectorl(-8, 4), Vectorl(-6, 7)),Vector(0, 0, 0, 0))
    val players = Vector(StrikeBackPlayer(Vector(0, 1), Vector(2, 3)), BestStrikeBackPlayer(Vector(2, 3)))
    val result = GameSimulator.simulate(1, state, StrikeBackArena, players)
    println(result)
  }

}
