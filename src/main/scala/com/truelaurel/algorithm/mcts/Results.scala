package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game.{Outcome, Wins}

// score is delta of trueWins - falseWins
case class Results(played: Int = 0,
                   score: Int = 0) {

  def withOutcome(outcome: Outcome[Boolean]): Results = copy(
    played = played + 1,
    score = score + (if (outcome == Wins(true)) 1 else if (outcome == Wins(false)) -1 else 0)
  )

  def uct(total: Int, next: Boolean): Double = {
    if (played == 0) Double.MaxValue
    else wins(next) / played + math.sqrt(2 * math.log(total) / played)
  }


  def wins(player: Boolean): Double =
    (played + (if (player) score else -score)) / 2.0


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
  def mostPromisingMove[M](next: Boolean, results: Iterable[(M, Results)]): M = {
    val total = results.map(_._2.played).sum
    mostPromisingMove(next, results, total)
  }

  def mostPromisingMove[M](next: Boolean, results: Iterable[(M, Results)], total: Int): M = {
    results.maxBy(_._2.uct(total, next))._1
  }
}