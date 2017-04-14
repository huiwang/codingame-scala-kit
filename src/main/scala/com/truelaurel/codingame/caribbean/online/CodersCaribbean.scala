package com.truelaurel.codingame.caribbean.online

import com.truelaurel.codingame.caribbean.common._
import com.truelaurel.codingame.caribbean.head.CabribbeanPlayer
import com.truelaurel.codingame.engine.{GameController, GameLoop}

import scala.io.StdIn

/**
  * Created by hwang on 14/04/2017.
  */
object Player  {

  val gameLoop = new GameLoop(CaribbeanController, CabribbeanPlayer(1))

  def main(args: Array[String]): Unit = {
    gameLoop.run()
  }
}
