package com.truelaurel.samplegames.stirfry

import com.truelaurel.algorithm.mcts.MctsAi
import com.truelaurel.time.Chronometer

import scala.concurrent.duration.DurationInt

object FryDemo {
  val count = 3
  val rules = FryRules(count)
  val players = (0 until count).map(p => p -> mctsMove _).toMap

  def main(args: Array[String]): Unit = {
    val outcome = rules.judge(players, s => println(s))
    println(outcome)
  }

  def randomMove(s: FryBoard): FryMove = {
    val move = rules.randomMove(s)
    println(move)
    move
  }

  def mctsMove(s: FryBoard): FryMove = {
    val chronometer = new Chronometer(2.seconds)
    chronometer.start()
    val move = MctsAi(rules)(_ => chronometer.willOutOfTime).chooseMove(s)
    println(move)
    move
  }

}
