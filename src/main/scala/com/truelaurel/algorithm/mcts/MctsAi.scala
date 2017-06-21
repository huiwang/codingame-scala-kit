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
//        println(node.debugText)
        node.bestMove
      } else iterate(node.step)

    iterate(makeNode(state))
  }

  def makeNode(state: S): MctsNode[S, M] = {
    MctsNode(state, rules, randomPlay)
  }


}
