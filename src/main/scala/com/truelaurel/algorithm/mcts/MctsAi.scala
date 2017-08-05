package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game.{GameRules, GameState, Outcome}

import scala.annotation.tailrec

case class MctsAi[P, S <: GameState[P], M](rules: GameRules[P, S, M])
                                          (stopCondition: MctsNode[P, S, M] => Boolean,
                                           randomPlay: S => Outcome[P] = rules.randomPlay _) {

  def chooseMove(state: S): M = {
    @tailrec
    def iterate(node: MctsNode[P, S, M]): M =
      if (stopCondition(node)) {
        node.bestMove
      } else iterate(node.step)

    iterate(makeNode(state))
  }

  def chooseMoveCount(state: S): (M, Int) = {
    @tailrec
    def iterate(node: MctsNode[P, S, M], nodes: Int = 0): (M, Int) =
      if (stopCondition(node)) {
        (node.bestMove, nodes)
      } else iterate(node.step, nodes + 1)

    iterate(makeNode(state))
  }

  def makeNode(state: S):  MctsNode[P, S, M] =
    MctsNode[P, S, M](state, rules, randomPlay)


}
