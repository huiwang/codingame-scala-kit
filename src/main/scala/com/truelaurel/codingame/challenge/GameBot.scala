package com.truelaurel.codingame.challenge

/**
  * Created by hwang on 09/07/2017.
  */
trait GameBot[State, Action] {
  /**
    * Reacts to the given game state by playing one or more actions
    *
    * @param state current state of the game
    * @return one or more actions to play
    */
  def react(state: State): Action
}
