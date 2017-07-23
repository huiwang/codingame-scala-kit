package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.simulation.WondevSimulator
import com.truelaurel.samplegames.wondev.domain.{FastWondevState, WondevAction, WondevContext, WondevState}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hwang on 15/07/2017.
  */
object WarFogCleaner {


  def restrictOppoScope(observedRaw: WondevState,
                        previousStateRaw: WondevState,
                        previousAction: WondevAction,
                        previousOppoScope: Set[Set[Pos]]): Set[Set[Pos]] = {
    val observed = FastWondevState.fromSlowState(observedRaw)
    if (previousAction == null) {
      val oppoUnits = observed.readable.opUnits
      val visibleOppo = oppoUnits.filter(WondevContext.isVisible)
      val myUnits = observed.readable.myUnits
      val occupables = observed.readable.feasibleOppo()
      val oppoScope = occupables.subsets(2).toSet.filter(set => hasSameUnvisibleOppo(2 - visibleOppo.length, set, myUnits))
      val increased = observed.readable.findIncreasedCell
      if (increased.isDefined) {
        //oppo started the game
        oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains)
          && oppoSet.exists(_.distance(increased.get) == 1))
      } else {
        oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains))
      }
    } else {
      val previousState = FastWondevState.fromSlowState(previousStateRaw)
      val restricted = collection.mutable.ArrayBuffer.empty[Set[Pos]]
      val previousScope = previousOppoScope.toArray
      val observedOppo = observed.readable.opUnits
      val visibleOppo = observedOppo.filter(WondevContext.isVisible)
      val observedSelf = observed.readable.myUnits
      var i = 0
      while (i < previousScope.length) {
        val oppoSet = previousScope(i)
        previousState.writable.start()
        val oppoUpdated = previousState.writable.setOppo(oppoSet)
        previousState.writable.end()
        val myActionApplied = WondevSimulator.next(oppoUpdated, previousAction)
        val oppoLegalActions = WondevSimulator.nextLegalActions(previousState)
        findConsistentState(oppoLegalActions, myActionApplied, observed, visibleOppo, observedSelf, observedOppo, restricted)
        myActionApplied.writable.undo()
        oppoUpdated.writable.undo()
        i += 1
      }
      if (restricted.isEmpty) previousOppoScope else restricted.toSet
    }
  }


  def findConsistentState(legalActions: Seq[WondevAction],
                          myActionApplied: FastWondevState,
                          observed: FastWondevState,
                          visibleOppo: Array[Pos],
                          observedSelf: Array[Pos],
                          observedOppo: Array[Pos],
                          restricted: ArrayBuffer[Set[Pos]]): Unit = {
    var i = 0
    while (i < legalActions.size) {
      val action = legalActions(i)
      val simulated = WondevSimulator.next(myActionApplied, action)
      if (consistent(simulated, observed, visibleOppo, observedSelf, observedOppo)) {
        restricted.append(simulated.readable.opUnits.toSet)
      }
      simulated.writable.undo()
      i += 1
    }
  }

  def consistent(simulated: FastWondevState, observed: FastWondevState, visibleOppo: Array[Pos], observedSelf: Array[Pos], observedOppo: Array[Pos]): Boolean = {
    val simulatedOppo = simulated.readable.opUnits
    val simulatedSelf = simulated.readable.myUnits
    (observedSelf sameElements simulatedSelf) &&
      visibleOppo.forall(simulatedOppo.contains) &&
      hasSameUnvisibleOppo(2 - visibleOppo.length, simulatedOppo, observedSelf) &&
      observed.readable.hasSameHeightMap(simulated)
  }

  private def hasSameUnvisibleOppo(size : Int, simulatedOppo: Iterable[Pos], observedSelf: Iterable[Pos]) = {
     simulatedOppo.count(pos => observedSelf.forall(_.distance(pos) > 1)) == size
  }

  def removeFog(state: WondevState, oppoScope: Set[Set[Pos]]): WondevState = {
    state.copy(units = state.units.take(2) ++ oppoScope.head.toSeq)
  }

}
