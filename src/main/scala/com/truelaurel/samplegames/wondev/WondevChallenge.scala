package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WarFogCleaner
import com.truelaurel.samplegames.wondev.simulation.WondevSimulator
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevIO
import com.truelaurel.samplegames.wondev.strategy.MinimaxPlayer
import com.truelaurel.samplegames.wondev.warmup.WondevWarmup

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevChallenge.scala
// cp -r target/universal/stage/ brutaltester/head

// java -jar cg-brutaltester.jar -r "java -jar cg-ww.jar" -p1 "./head/bin/player" -p2 "./best/bin/player" -t 2 -n 100 -l "./logs/"
// cp -rf head/ best/

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info

    var turn = 0
    var context = WondevIO.readContext

    val time = System.nanoTime()
    for (i <- 0 until 5) {
      WondevWarmup.warmup()
    }
    System.err.println(
      "warmup elt: " + (System.nanoTime() - time) / 1000000 + "ms"
    )

    try {
      while (true) {
        val state = WondevIO.readState(turn, context)

        val start = System.nanoTime()
        val predictedActions = WondevSimulator.nextLegalActions(state)
        if (state.legalActions.toSet != predictedActions.toSet) {
          CGLogger.info("action prediction not working")
        }

        CGLogger.info(context)
        CGLogger.info(state)
        val restrictedOppoScope =
          WarFogCleaner.restrictOppoScope(state, context)

        val clearedState = WarFogCleaner.removeFog(state, restrictedOppoScope)
        val action = MinimaxPlayer.react(clearedState)
        context = context.copy(
          previousState = state,
          previousAction = action,
          previousOppoScope = restrictedOppoScope
        )
        WondevIO.writeAction(state, action)
        turn += 1
      }
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}
