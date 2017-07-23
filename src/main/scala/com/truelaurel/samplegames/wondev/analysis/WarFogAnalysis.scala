package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.arena.UndoWondevArena
import com.truelaurel.samplegames.wondev.domain.{FastWondevState, WondevAction, WondevContext, WondevState}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hwang on 15/07/2017.
  */
object WarFogAnalysis {


  def restrictOppoScope(observedRaw: WondevState,
                        previousStateRaw: WondevState,
                        previousAction: WondevAction,
                        previousOppoScope: Set[Set[Pos]]): Set[Set[Pos]] = {
    val observed = FastWondevState.fromSlowState(observedRaw)
    val myUnits = observed.readable.myUnits
    val oppoUnits = observed.readable.opUnits
    val visibleOppo = oppoUnits.filter(WondevAnalysis.isVisible)
    if (previousAction == null) {
      val occupables = observed.readable.feasibleOppo()
      val oppoScope = occupables.subsets(2).toSet.filter(set => hasSameUnvisibleOppo(oppoUnits, set, myUnits))
      val increased = observed.readable.findIncreasedCell
      if (increased.isDefined) {
        //oppo started the game
        oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains)
          && oppoSet.exists(_.distance(increased.get) == 1))
      } else {
        oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains))
      }
    }  else {
      val previousState = FastWondevState.fromSlowState(previousStateRaw)
      val restricted = collection.mutable.ArrayBuffer.empty[Set[Pos]]
      val previousScope = previousOppoScope.toArray
      var i = 0
      while (i < previousScope.length) {
        val oppoSet = previousScope(i)
        previousState.undoable.start()
        val oppoUpdated = previousState.undoable.setOppo(oppoSet)
        previousState.undoable.end()
        val myActionApplied = UndoWondevArena.next(oppoUpdated, previousAction)
        val oppoLegalActions = UndoWondevArena.nextLegalActions(previousState)
        findConsistentState(oppoLegalActions, myActionApplied, observed, restricted)
        myActionApplied.undoable.undo()
        oppoUpdated.undoable.undo()
        i += 1
      }
      if (restricted.isEmpty) previousOppoScope else restricted.toSet
    }
  }


  def findConsistentState(legalActions: Seq[WondevAction], myActionApplied: FastWondevState, observed: FastWondevState, restricted: ArrayBuffer[Set[Pos]]): Unit = {
    var i = 0
    while (i < legalActions.size) {
      val action = legalActions(i)
      val simulated = UndoWondevArena.next(myActionApplied, action)
      if (consistent(simulated, observed)) {
        restricted.append(simulated.readable.opUnits.toSet)
      }
      simulated.undoable.undo()
      i += 1
    }
  }

  def consistent(simulated: FastWondevState, observed: FastWondevState): Boolean = {
    val observedOppo = observed.readable.opUnits
    val simulatedOppo = simulated.readable.opUnits
    val observedSelf = observed.readable.myUnits
    val simulatedSelf = simulated.readable.myUnits
    (observedSelf sameElements simulatedSelf) &&
      observedOppo.filter(WondevAnalysis.isVisible).forall(simulatedOppo.contains) &&
      hasSameUnvisibleOppo(observedOppo, simulatedOppo, observedSelf) &&
      observed.readable.hasSameHeightMap(simulated)
  }

  private def hasSameUnvisibleOppo(observedOppo: Iterable[Pos], simulatedOppo: Iterable[Pos], observedSelf: Iterable[Pos]) = {
    observedOppo.count(pos => !WondevAnalysis.isVisible(pos)) == simulatedOppo.count(pos => observedSelf.forall(_.distance(pos) > 1))
  }

  def removeFog(state: WondevState, oppoScope: Set[Set[Pos]]): WondevState = {
    state.copy(units = state.units.take(2) ++ oppoScope.head.toSeq)
  }

}
