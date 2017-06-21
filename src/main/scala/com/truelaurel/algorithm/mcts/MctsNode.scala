package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game._

import scala.annotation.tailrec


case class MctsNode[State <: GameState[Boolean], Move](state: State,
                                                       rules: RulesFor2p[State, Move],
                                                       randomPlay: State => Outcome[Boolean],
                                                       results: Results = Results(),
                                                       children: Map[Move, MctsNode[State, Move]] = Map.empty[Move, MctsNode[State, Move]]) {

  def bestMove: Move =
    children.mapValues(_.results.played).maxBy(_._2)._1

  @tailrec
  final def steps(count: Int): MctsNode[State, Move] =
    if (count == 0) this
    else step.steps(count - 1)

  def step: MctsNode[State, Move] =
    deepStep._2

  def moveToExplore: Move = {
    val validMoves = rules.validMoves(state)
    validMoves.find(m => children.get(m).isEmpty) match {
      case Some(unexploredMove) => unexploredMove
      case None =>
        val movesResults = validMoves.map(m => m -> children(m).results)
        Results.mostPromisingMove(state.nextPlayer, movesResults, results.played)
    }
  }

  def debugText: String = {
    for {
      (move, n) <- children
      r@Results(played, _) = n.results
      wins = r.wins(state.nextPlayer).toInt
    } yield s"$wins/$played for $move\n"
  }.mkString

  private def deepStep: (Outcome[Boolean], MctsNode[State, Move]) = {
    rules.outcome(state) match {
      case Undecided =>
        val move = moveToExplore
        val (outcome, updatedChild) = children.get(move) match {
          case Some(child) =>
            child.deepStep
          case None =>
            expand(move)
        }
        (outcome, copy(
          results = results.withOutcome(outcome),
          children = children + (move -> updatedChild)))
      case decided =>
        (decided, copy(results = results.withOutcome(decided)))
    }
  }

  private def expand(move: Move): (Outcome[Boolean], MctsNode[State, Move]) = {
    val nextState = rules.applyMove(state, move)
    val outcome = rules.outcome(nextState) match {
      case Undecided => randomPlay(nextState)
      case decided => decided
    }
    val childNode = copy(
      state = nextState,
      results = Results().withOutcome(outcome),
      children = Map.empty[Move, MctsNode[State, Move]])
    (outcome, childNode)
  }
}

object MctsNode {
  def apply[State <: GameState[Boolean], Move](state: State, rules: RulesFor2p[State, Move]): MctsNode[State, Move] =
    MctsNode(
      state,
      rules,
      rules.randomPlay
    )
}
