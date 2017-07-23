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
    val initContext = WondevIO.readContext
    var previousState: WondevState = null
    var previousAction: WondevAction = null
    var previousOppoScope: Set[Set[Pos]] = null

    val time = System.nanoTime()
    for ( i <- 0 until 5) {
      WondevWarmup.warmup()
    }
    System.err.println("warmup elt: " + (System.nanoTime() - time) / 1000000 + "ms")

    try {
      while (true) {
        val state = WondevIO.readState(turn, initContext)

        val start = System.nanoTime()
        val predictedActions = WondevSimulator.nextLegalActions(state)
        if (state.legalActions.toSet != predictedActions.toSet) {
          CGLogger.info("action prediction not working")
        }

        CGLogger.info(state)

        CGLogger.info("previous action " + previousAction)
        CGLogger.info("previous oppo scope " + previousOppoScope)
        val oppoScope = WarFogCleaner.restrictOppoScope(state, previousState, previousAction, previousOppoScope)

        CGLogger.info("restricted " + oppoScope)
        CGLogger.info("Elapsed " + (System.nanoTime() - start) / 1000000)
        val clearedState = WarFogCleaner.removeFog(state, oppoScope)
        val action = MinimaxPlayer.react(clearedState)
        previousState = state
        previousAction = action
        previousOppoScope = oppoScope

        WondevIO.writeAction(state, action)
        turn += 1
      }
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}

