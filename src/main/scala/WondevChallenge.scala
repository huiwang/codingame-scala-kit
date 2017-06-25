import com.truelaurel.codingame.challenge.GameLoop
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.samplegames.wondev.io.WondevController
import com.truelaurel.samplegames.wondev.strategy.WondevPlayer

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevChallenge.scala

/**
  * TODO how to figure out op unit position
  * - from push result
  * - from build result
  * - from height distribution
  */

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info
    val gameLoop = new GameLoop(WondevController, WondevPlayer(true))
    gameLoop.run()
  }
}

