package com.truelaurel.algorithm.mcts

import com.truelaurel.algorithm.game._

import scala.annotation.tailrec


case class MctsNode[P, State <: GameState[P], Move](state: State,
                                                    rules: GameRules[P, State, Move],
                                                    randomPlay: State => Outcome[P],
                                                    results: Results = Results(),
                                                    children: Map[Move, MctsNode[P, State, Move]] = Map.empty[Move, MctsNode[P, State, Move]]) {

  def bestMove: Move =
    children.mapValues(_.results.played).maxBy(_._2)._1

  @tailrec
  final def steps(count: Int): MctsNode[P, State, Move] =
    if (count == 0) this
    else step.steps(count - 1)

  def step: MctsNode[P, State, Move] =
    deepStep._2

  def moveToExplore: Move = {
    val validMoves = rules.validMoves(state)
    validMoves.find(m => children.get(m).isEmpty) match {
      case Some(unexploredMove) => unexploredMove
      case None =>
        val movesResults = validMoves.map(m => m -> children(m).results)
        Results.mostPromisingMove(true, movesResults, results.played)
    }
  }

  def debugText: String = {
    for {
      (move, n) <- children
      r@Results(played, _) = n.results
      wins = r.wins(true).toInt
    } yield s"$wins/$played for $move\n"
  }.mkString

  private def deepStep: (Outcome[P], MctsNode[P, State, Move]) = {
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
          results = results.withOutcome(state.nextPlayer, outcome),
          children = children + (move -> updatedChild)))
            case decided =>
              (decided, copy(results = results.withOutcome(state.nextPlayer,decided)))
    }
  }

  private def expand(move: Move): (Outcome[P], MctsNode[P, State, Move]) = {
    val nextState = rules.applyMove(state, move)
    val outcome = rules.outcome(nextState) match {
      case Undecided => randomPlay(nextState)
      case decided => decided
    }
    val childNode = copy(
      state = nextState,
      results = Results().withOutcome(state.nextPlayer,outcome),
      children = Map.empty[Move, MctsNode[P, State, Move]])
    (outcome, childNode)
  }
}


