package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WarFogAnalysis
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevIO
import com.truelaurel.samplegames.wondev.strategy.MinimaxPlayer

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
    try {
      while (true) {
        val state = WondevIO.readState(turn, initContext)

        val predictedActions = WondevArena.nextLegalActions(state)
        if (state.legalActions.toSet != predictedActions.toSet) {
          CGLogger.info("action prediction not working")
        }

        CGLogger.info(state)

        CGLogger.info("previous action " + previousAction)
        CGLogger.info("previous oppo scope " + previousOppoScope)
        val oppoScope = WarFogAnalysis.restrictOppoScope(state, previousState, previousAction, previousOppoScope)

        CGLogger.info("restricted " + oppoScope)
        val clearedState = WarFogAnalysis.removeFog(state, oppoScope)
        val action = MinimaxPlayer.react(clearedState)
        previousState = state
        previousAction = action
        previousOppoScope = oppoScope

        WondevIO.writeAction(state, action)
        turn += 1
      }
    } catch {
      case e => e.printStackTrace()
    }
  }
}

