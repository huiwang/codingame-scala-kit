package ai.scala.fp.game

trait GameState[P] {
  def nextPlayer: P
}
