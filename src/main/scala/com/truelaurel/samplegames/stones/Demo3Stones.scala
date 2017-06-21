package com.truelaurel.samplegames.stones

import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.samplegames.gomoku.GomokuBoard
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object Demo3Stones {

  def main(args: Array[String]): Unit = {
    val outcome = Rules.judge(
      truePl = aiMove,
      falsePl = aiMove,
      s => println(s.toText))
    println(outcome)
  }

  def aiMove(s: GomokuBoard): Move = {
    val chronometer = new Chronometer(5.seconds)
    chronometer.start()
    MctsAi(Rules)(_ => chronometer.willOutOfTime).chooseMove(s)
  }
}
