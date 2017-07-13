package com.truelaurel.codingame.challenge

trait GameSimulator[State, Action] {
  /**
    * Impacts the provided action on the given state and
    * produces a new state according to the defined game rule
    *
    * @param state   the starting state
    * @param action action selected based on the starting state
    * @return an updated state after actions impact
    */
  def simulate(state: State, action: Action): State

}

