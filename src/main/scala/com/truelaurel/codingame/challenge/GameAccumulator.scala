package com.truelaurel.codingame.challenge

/**
  * Created by hwang on 09/07/2017.
  */
trait GameAccumulator[State, Action] {

  /**
    * Accumulates information derived from the current state and selected actions into a new game context that will be
    * used in the next round.
    *
    * In certain cases, the input state doesn't include all known information. These information must be calculated from
    * historical actions and states. For example, it could action cool down, previously observed positions in fog of war.
    *
    * @param state   the current state
    * @param actions actions performed for the current round
    * @return a new state accumulated with historical events including those generated from the current round
    */
  def accumulate(state: State, actions: Vector[Action]): State
}

