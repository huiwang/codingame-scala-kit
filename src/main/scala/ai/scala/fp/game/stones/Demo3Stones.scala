package ai.scala.fp.game.stones

import ai.scala.fp.game.gomoku.GomokuBoard
import ai.scala.fp.game.mcts.MctsAi
import com.truelaurel.codingame.time.Chronometer

import scala.concurrent.duration.DurationInt

object Demo3Stones {

  def aiMove(s: GomokuBoard): Move = {
    val chronometer = new Chronometer(5.seconds)
    chronometer.start()
    MctsAi(Rules)(_ => chronometer.willOutOfTime).chooseMove(s)
  }

  def main(args: Array[String]): Unit = {
    val outcome = Rules.judge(
      truePl = aiMove,
      falsePl = aiMove,
      s => println(s.toText))
    println(outcome)
  }
}
