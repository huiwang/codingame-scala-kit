package com.truelaurel.samplegames.wondev

import com.truelaurel.codingame.logging.CGLogger
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WarFogAnalysis
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain.{MoveBuild, WondevAction, WondevState}
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

    val warmupState = WondevState(5,
      heightMap = Map(
        Pos(0, 2) -> 0, Pos(0, 0) -> 0, Pos(4, 0) -> 0, Pos(3, 4) -> 0, Pos(3, 1) -> 0,
        Pos(4, 1) -> 0, Pos(2, 0) -> 0, Pos(0, 3) -> 0, Pos(4, 4) -> 0, Pos(3, 0) -> 0,
        Pos(1, 1) -> 0, Pos(1, 4) -> 0, Pos(0, 4) -> 0, Pos(3, 2) -> 0, Pos(1, 3) -> 0,
        Pos(2, 2) -> 0, Pos(4, 2) -> 0, Pos(2, 4) -> 0, Pos(0, 1) -> 0, Pos(3, 3) -> 0,
        Pos(2, 3) -> 0, Pos(1, 2) -> 0, Pos(2, 1) -> 0, Pos(4, 3) -> 0, Pos(1, 0) -> 0),
      units = List(Pos(0, 4), Pos(1, 0), Pos(0, 0), Pos(-1, -1)),
      legalActions = List(
        MoveBuild(0, Pos(1, 4), Pos(2, 4)), MoveBuild(0, Pos(1, 4), Pos(1, 3)), MoveBuild(0, Pos(1, 4), Pos(2, 3)),
        MoveBuild(0, Pos(1, 4), Pos(0, 3)), MoveBuild(0, Pos(1, 4), Pos(0, 4)), MoveBuild(0, Pos(0, 3), Pos(1, 3)),
        MoveBuild(0, Pos(0, 3), Pos(0, 2)), MoveBuild(0, Pos(0, 3), Pos(1, 2)), MoveBuild(0, Pos(0, 3), Pos(0, 4)),
        MoveBuild(0, Pos(0, 3), Pos(1, 4)), MoveBuild(0, Pos(1, 3), Pos(2, 3)), MoveBuild(0, Pos(1, 3), Pos(1, 2)),
        MoveBuild(0, Pos(1, 3), Pos(2, 2)), MoveBuild(0, Pos(1, 3), Pos(0, 2)), MoveBuild(0, Pos(1, 3), Pos(1, 4)),
        MoveBuild(0, Pos(1, 3), Pos(2, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 4)), MoveBuild(0, Pos(1, 3), Pos(0, 3)),
        MoveBuild(1, Pos(2, 0), Pos(3, 0)), MoveBuild(1, Pos(2, 0), Pos(2, 1)), MoveBuild(1, Pos(2, 0), Pos(3, 1)),
        MoveBuild(1, Pos(2, 0), Pos(1, 1)), MoveBuild(1, Pos(2, 0), Pos(1, 0)), MoveBuild(1, Pos(1, 1), Pos(2, 1)),
        MoveBuild(1, Pos(1, 1), Pos(1, 0)), MoveBuild(1, Pos(1, 1), Pos(2, 0)), MoveBuild(1, Pos(1, 1), Pos(1, 2)),
        MoveBuild(1, Pos(1, 1), Pos(2, 2)), MoveBuild(1, Pos(1, 1), Pos(0, 2)), MoveBuild(1, Pos(1, 1), Pos(0, 1)),
        MoveBuild(1, Pos(2, 1), Pos(3, 1)), MoveBuild(1, Pos(2, 1), Pos(2, 0)), MoveBuild(1, Pos(2, 1), Pos(3, 0)),
        MoveBuild(1, Pos(2, 1), Pos(1, 0)), MoveBuild(1, Pos(2, 1), Pos(2, 2)), MoveBuild(1, Pos(2, 1), Pos(3, 2)),
        MoveBuild(1, Pos(2, 1), Pos(1, 2)), MoveBuild(1, Pos(2, 1), Pos(1, 1)), MoveBuild(1, Pos(0, 1), Pos(1, 1)),
        MoveBuild(1, Pos(0, 1), Pos(1, 0)), MoveBuild(1, Pos(0, 1), Pos(0, 2)), MoveBuild(1, Pos(0, 1), Pos(1, 2))),
      nextPlayer = true)

    WarFogAnalysis.restrictOppoScope(warmupState, previousState, previousAction, previousOppoScope)
    MinimaxPlayer.react(warmupState)


    try {
      while (true) {
        val start = System.nanoTime()
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
        CGLogger.info("Elapsed " + (System.nanoTime() - start) / 1000000)
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

