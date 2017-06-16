package ai.scala.fp.game.gomoku

import ai.scala.fp.game.alphabeta.AlphaBetaAi
import ai.scala.fp.game.mcts.MctsAi
import ai.scala.fp.geo.Pos
import com.truelaurel.codingame.time.Chronometer

import scala.concurrent.duration.DurationInt

object GomokuDemo {
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

  val rules = GomokuRules(7, 4)

  def alphaBetaMove(s: GomokuBoard): Pos =
    AlphaBetaAi(rules, rules.centerHeuristic).chooseMove(s, 3)


}
