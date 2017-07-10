import com.truelaurel.codingame.challenge.GameLoop
import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevIO
import com.truelaurel.samplegames.wondev.strategy.WondevBot

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevChallenge.scala

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info
    GameLoop.run(
      WondevIO.readInitialState,
      WondevIO.readState,
      WondevIO.writeAction,
      WondevBot(true).react)
  }
}

