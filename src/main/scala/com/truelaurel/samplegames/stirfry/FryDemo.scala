package com.truelaurel.samplegames.stirfry

object FryDemo {
  val count = 3
  val rules = FryRules(count)
  val players = (0 until count).map(p => p -> randomMove _).toMap

  def main(args: Array[String]): Unit = {
    val outcome = rules.judge(players, s => println(s))
    println(outcome)
  }

  def randomMove(s: FryBoard): FryMove = {
    val move = rules.randomMove(s)
    println(move)
    move
  }

}
