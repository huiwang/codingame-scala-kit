package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game.{GameState, Outcome, RulesFor2p}

import scala.annotation.tailrec

case class MctsAi[S <: GameState[Boolean], M](rules: RulesFor2p[S, M])
                                             (stopCondition: MctsNode[S, M] => Boolean,
                                              randomPlay: S => Outcome[Boolean] = rules.randomPlay _) {

  def chooseMove(state: S): M = {
    @tailrec
    def iterate(node: MctsNode[S, M]): M =
      if (stopCondition(node)) {
        node.bestMove
      } else iterate(node.step)

    iterate(makeNode(state))
  }

  def chooseMoveCount(state: S): (M, Int) = {
    @tailrec
    def iterate(node: MctsNode[S, M], nodes: Int = 0): (M, Int) =
      if (stopCondition(node)) {
        (node.bestMove, nodes)
      } else iterate(node.step, nodes + 1)

    iterate(makeNode(state))
  }

  def makeNode(state: S): MctsNode[S, M] = {
    MctsNode(state, rules, randomPlay)
  }


}
