package ai.scala.fp.game

import scala.annotation.tailrec
import scala.util.Random

trait GameRules[P, S <: GameState[P], M] {
  def initial: S

  def validMoves(state: S): Seq[M]

  def applyMove(state: S, move: M): S

  def outcome(state: S): Outcome[P]

  def playUntilEnd(selectMove: S => M)(state: S): Outcome[P] = {
    @tailrec
    def playRec(s: S): Outcome[P] = {
      outcome(s) match {
        case Undecided => playRec(applyMove(s, selectMove(s)))
        case decided => decided
      }
    }

    playRec(state)
  }

  def randomPlay(state: S): Outcome[P] = {
    def randomMove(s: S): M = {
      val moves = validMoves(s)
      require(moves.nonEmpty, "no valid moves in state " + s)
      moves(Random.nextInt(moves.size))
    }

    playUntilEnd(randomMove)(state)
  }
}