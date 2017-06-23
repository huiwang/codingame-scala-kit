import com.truelaurel.codingame.challenge.GameLoop
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.samplegames.wondev.WondevController
import com.truelaurel.samplegames.wondev.strategy.NilPlayer

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevPlayer.scala

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info
    val gameLoop = new GameLoop(WondevController, NilPlayer(true))
    gameLoop.run()
  }
}

