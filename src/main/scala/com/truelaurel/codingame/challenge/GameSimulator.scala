package com.truelaurel.codingame.challenge

trait GameSimulator[State, Action] {
  /**
    * Impacts the provided actions on the given state and
    * produces a new state according to the defined game rule
    *
    * @param state   the starting state
    * @param actions actions selected based on the starting state
    * @return an updated state after actions impact
    */
  def simulate(state: State, actions: Vector[Action]): State

}

