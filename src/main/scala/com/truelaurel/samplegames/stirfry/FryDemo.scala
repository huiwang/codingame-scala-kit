package com.truelaurel.samplegames.stirfry

object FryDemo {
  val rules = FryRules(2)

  def main(args: Array[String]): Unit = {
    val outcome = rules.judge(
      Map(0 -> randomMove, 1 -> randomMove),
      s => println(s))
    println(outcome)
  }

  def randomMove(s: FryBoard): FryMove =
    rules.randomMove(s)

}
