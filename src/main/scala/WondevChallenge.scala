import com.truelaurel.codingame.challenge.GameLoop
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.samplegames.wondev.io.FastController
import com.truelaurel.samplegames.wondev.strategy.WondevFastPlayer

import scala.io.StdIn.readInt

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevChallenge.scala

/**
  * TODO how to figure out op unit position
  * - from push result
  * - from build result
  * - from height distribution
  */

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.debug
    val size = readInt
    val gameLoop = new GameLoop(FastController(size), WondevFastPlayer(true, size))
    gameLoop.run()
  }
}

