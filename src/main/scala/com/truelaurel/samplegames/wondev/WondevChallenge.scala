package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WarFogAnalysis
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevState}
import com.truelaurel.samplegames.wondev.io.WondevIO
import com.truelaurel.samplegames.wondev.strategy.MinimaxPlayer

// ~runMain com.truelaurel.codingame.tool.bundle.BundlerMain WondevChallenge.scala

object Player {
  def main(args: Array[String]): Unit = {
    CGLogger.current = CGLogger.info

    var turn = 0
    val initContext = WondevIO.readContext
    var previousState: WondevState = null
    var previousAction: WondevAction = null
    var previousOppoScope: Iterator[Set[Pos]] = null
    while (true) {
      val state = WondevIO.readState(turn, initContext)
      CGLogger.info(state)
      val oppoScope = WarFogAnalysis.restrictOppoScope(state, previousState, previousAction, previousOppoScope)
      val clearedState = WarFogAnalysis.removeFog(state, oppoScope)
      val action = MinimaxPlayer.react(clearedState)
      previousState = state
      previousAction = action
      previousOppoScope = oppoScope

      WondevIO.writeAction(state, action)
      turn += 1
    }
  }
}

