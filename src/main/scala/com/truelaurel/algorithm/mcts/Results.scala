package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game.{Outcome, Wins}

// score is delta of myWins - otherWins
case class Results(played: Int = 0, score: Int = 0) {

  def withOutcome(outcome: Outcome[Boolean]): Results =
    withOutcome(true, outcome)

  def withOutcome[P](player: P, outcome: Outcome[P]): Results =
    copy(
      played = played + 1,
      score = score + (outcome match {
        case Wins(`player`) => 1
        case Wins(_)        => -1
        case _              => 0
      })
    )

  def uct(total: Int): Double = {
    if (played == 0) Double.MaxValue
    else lost / played + math.sqrt(2 * math.log(total) / played)
  }

  def lost: Double = (played - score) / 2.0
  def won: Double = (played + score) / 2.0

  /*
Ex :
5 for true
2 for false
score = 3
played = 7


Demonstration :

s = t-f
p = t+f

t = s+f
t = p -f
s+f = p-f
2f = p-s
f = (p-s)/2
t = (p+s)/2

   */
}

object Results {
  // faster using a (sorted) TreeMap ?
  // results must be a non empty map
  def mostPromisingMove[M](results: Iterable[(M, Results)]): M = {
    val total = results.map(_._2.played).sum
    results.maxBy(_._2.uct(total))._1
  }
}
