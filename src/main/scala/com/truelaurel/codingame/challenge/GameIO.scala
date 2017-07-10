package com.truelaurel.codingame.challenge

/**
  * Created by hwang on 09/07/2017.
  */
trait GameIO[State, Action] {
  /**
    * Reads game context from the referee system. A context stores game's global information
    */
  def readInitialState: State

  /**
    * Reads current state from the referee system. A state provides information for the current turn
    */
  def readState(turn: Int, previous: State): State

  /**
    * Writes action to the referee system
    */
  def writeAction(action: Action)
}