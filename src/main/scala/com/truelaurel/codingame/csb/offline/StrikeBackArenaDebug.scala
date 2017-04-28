package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.common.StrikeBackGameState
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 28/04/2017.
  */
object StrikeBackArenaDebug {
  def main(args: Array[String]): Unit = {
    val state = StrikeBackGameState(Vector(Disk(Vectorl(13930.0, 1925.0), Vectorl(0.0, 0.0), 200.0, 1.0), Disk(Vectorl(8026.0, 3273.0), Vectorl(0.0, 0.0), 200.0, 1.0), Disk(Vectorl(2662.0, 6991.0), Vectorl(0.0, 0.0), 200.0, 1.0), Disk(Vectorl(10020.0, 5967.0), Vectorl(0.0, 0.0), 200.0, 1.0)), Vector(Disk(Vectorl(14041.0, 2412.0), Vectorl(0.0, 0.0), 400.0, 1.0), Disk(Vectorl(13819.0, 1438.0), Vectorl(0.0, 0.0), 400.0, 1.0), Disk(Vectorl(14264.0, 3387.0), Vectorl(0.0, 0.0), 400.0, 1.0), Disk(Vectorl(13596.0, 463.0), Vectorl(0.0, 0.0), 400.0, 1.0)), Vector(Vectorl(-0.9899099573973741, 0.14169783430077126), Vectorl(-0.9533161724973302, 0.3019739645317799), Vectorl(-0.9998330523927659, -0.018272037187043174), Vectorl(-0.8928185400193172, 0.4504165345519356)), Vector(1, 1, 1, 1))
    val players = Vector(StrikeBackPlayer(Vector(0, 1), Vector(2, 3)), BestStrikeBackPlayer(Vector(2, 3)))
    GameSimulator.evaluateOffline(Vector(state), StrikeBackArena, players)
  }
}
