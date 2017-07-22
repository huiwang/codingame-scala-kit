package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.arena.{UndoWondevArena, WondevArena}
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
    val myUnits = observed.myUnits
    val oppoUnits = observed.opUnits
    val visibleOppo = oppoUnits.filter(WondevAnalysis.isVisible)
    if (previousAction == null) {
      val occupables = observed.feasibleOppo()
      val oppoScope = occupables.subsets(2).toSet.filter(set => hasSameUnvisibleOppo(oppoUnits, set, myUnits))
      val result = oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains))
      result
    } else {
      val previousState = FastWondevState.fromSlowState(previousStateRaw)
      val restricted = collection.mutable.ArrayBuffer.empty[Set[Pos]]
      var i = 0
      val previousScope = previousOppoScope.toArray
      while (i < previousScope.length) {
        val oppoSet = previousScope(i)
        previousState.setOppo(oppoSet)
        val undo = UndoWondevArena.next(previousState, previousAction)
        val oppoLegalActions = UndoWondevArena.nextLegalActions(previousState)
        findConsistentState(oppoLegalActions, previousState, observed, restricted)
        i += 1
      }
      if (restricted.isEmpty) previousOppoScope else restricted.toSet
    }
  }


  def findConsistentState(legalActions: Seq[WondevAction], afterMe: FastWondevState, observed: FastWondevState, restricted: ArrayBuffer[Set[Pos]]): Unit = {
    var i = 0
    while (i < legalActions.size) {
      val action = legalActions(i)
      UndoWondevArena.next(afterMe, action)
      if (consistent(afterMe, observed)) {
        restricted.append(afterMe.opUnits.toSet)
      }
      i += 1
    }
  }

  def consistent(simulated: FastWondevState, observed: FastWondevState): Boolean = {
    val observedOppo = observed.opUnits
    val simulatedOppo = simulated.opUnits
    val observedSelf = observed.myUnits
    val simulatedSelf = simulated.myUnits
    (observedSelf sameElements simulatedSelf) &&
      observedOppo.filter(WondevAnalysis.isVisible).forall(simulatedOppo.contains) &&
      hasSameUnvisibleOppo(observedOppo, simulatedOppo, observedSelf) &&
      observed.hasSameHeightMap(simulated)
  }

  private def hasSameUnvisibleOppo(observedOppo: Iterable[Pos], simulatedOppo: Iterable[Pos], observedSelf: Iterable[Pos]) = {
    observedOppo.count(pos => !WondevAnalysis.isVisible(pos)) == simulatedOppo.count(pos => observedSelf.forall(_.distance(pos) > 1))
  }

  def removeFog(state: WondevState, oppoScope: Set[Set[Pos]]): WondevState = {
    state.copy(units = state.units.take(2) ++ oppoScope.head.toSeq)
  }

}
