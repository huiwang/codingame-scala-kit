package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.challenge.GameLoop
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.samplegames.wondev.io.WondevController
import com.truelaurel.samplegames.wondev.strategy.WondevPlayer

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevChallenge.scala

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info
    val gameLoop = new GameLoop(WondevController, WondevPlayer(true))
    gameLoop.run()
  }
}

