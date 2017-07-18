package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game._


class MctsNode2p[State <: GameState[Boolean], Move](state: State,
                                                    rules: RulesFor2p[State, Move],
                                                    randomPlay: State => Outcome[Boolean],
                                                    results: Results = Results(),
                                                    children: Map[Move, MctsNode2p[State, Move]] = Map.empty[Move, MctsNode2p[State, Move]])
  extends MctsNode[Boolean, State, Move](state, rules, randomPlay, results)


object MctsNode2p {
  def apply[State <: GameState[Boolean], Move](state: State, rules: RulesFor2p[State, Move]): MctsNode2p[State, Move] =
    new MctsNode2p(
      state,
      rules,
      rules.randomPlay
    )
}
