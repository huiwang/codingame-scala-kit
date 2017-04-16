package com.truelaurel.codingame.caribbean.online

import com.truelaurel.codingame.caribbean.best.BestCabribbeanPlayer
import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CaribbeanPlayer
import com.truelaurel.codingame.caribbean.offline.CaribbeanArena
import com.truelaurel.codingame.engine.{GameController, GameLoop, PredictableGameLoop}

import scala.io.StdIn

/**
  * Created by hwang on 14/04/2017.
  */
object Player {

  val gameLoop = new GameLoop(CaribbeanController, CaribbeanPlayer(1, 0))
  val predictableGameLoop = new PredictableGameLoop(CaribbeanController,
    CaribbeanPlayer(1, 0),
    BestCabribbeanPlayer(0, 1),
    CaribbeanArena)

  def main(args: Array[String]): Unit = {
    predictableGameLoop.run()
  }
}
