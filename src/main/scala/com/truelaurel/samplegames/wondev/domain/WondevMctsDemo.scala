package com.truelaurel.samplegames.wondev.domain

import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object WondevMctsDemo {
  val rules = WondevRules(5)

  def main(args: Array[String]): Unit = {
    val outcome = rules.judge(
      truePl = mctsMove,
      falsePl = mctsMove,
      s => println(s))
    println(outcome)
  }

  def mctsMove(s: FastState): WondevAction = {
    val chronometer = new Chronometer(1.seconds)
    chronometer.start()
    MctsAi(rules)(_ => chronometer.willOutOfTime).chooseMove(s)
  }


}
