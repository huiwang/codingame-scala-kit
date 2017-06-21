package com.truelaurel.samplegames.gomoku

import com.truelaurel.algorithm.alphabeta.AlphaBetaAi
import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.math.geometry.Pos
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object GomokuDemo {
  val rules = GomokuRules(7, 4)

  def main(args: Array[String]): Unit = {
    val outcome = rules.judge(
      truePl = mctsMove,
      falsePl = alphaBetaMove,
      s => println(s.toText))
    println(outcome)
  }

  def mctsMove(s: GomokuBoard): Pos = {
    val chronometer = new Chronometer(5.seconds)
    chronometer.start()
    MctsAi(rules)(_ => chronometer.willOutOfTime).chooseMove(s)
  }

  def alphaBetaMove(s: GomokuBoard): Pos =
    AlphaBetaAi(rules, rules.centerHeuristic).chooseMove(s, 3)


}
