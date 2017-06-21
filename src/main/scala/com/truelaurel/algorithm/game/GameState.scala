package com.truelaurel.algorithm.game

trait GameState[P] {
  def nextPlayer: P
}
