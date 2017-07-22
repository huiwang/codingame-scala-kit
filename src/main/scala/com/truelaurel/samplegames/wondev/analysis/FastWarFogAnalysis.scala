package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevContext, WondevState}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hwang on 15/07/2017.
  */
object FastWarFogAnalysis {


  def restrictOppoScope(observed: WondevState,
                        previousState: WondevState,
                        previousAction: WondevAction,
                        previousOppoScope: Set[Set[Pos]]): Set[Set[Pos]] = {
    val myUnits = observed.units.take(2)
    val oppoUnits = observed.units.takeRight(2)
    val visibleOppo = oppoUnits.filter(WondevAnalysis.isVisible)
    if (previousAction == null) {
      val occupables = observed.heightMap.keySet.filter(
        pos => WondevContext.isPlayable(observed.heightOf(pos)) &&
          myUnits.forall(myPos => myPos.distance(pos) > 0)
      )
      val oppoScope = occupables.subsets(2).toSet.filter(set => hasSameUnvisibleOppo(oppoUnits, set, myUnits))
      val increased = observed.heightMap.find(_._2 == 1)
      if (increased.isDefined) {
        //oppo started the game
        oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains)
          && oppoSet.exists(_.distance(increased.get._1) == 1))
      } else {
        oppoScope.filter(oppoSet => visibleOppo.forall(oppoSet.contains))
      }
    } else {
      val restricted = collection.mutable.ArrayBuffer.empty[Set[Pos]]
      var i = 0
      val previousScope = previousOppoScope.toArray
      while (i < previousScope.length) {
        val oppoSet = previousScope(i)
        val possibleState = previousState.copy(units = previousState.units.take(2) ++ oppoSet)
        val updatedStateByMe = WondevArena.next(possibleState, previousAction)
        val oppoLegalActions = WondevArena.nextLegalActions(updatedStateByMe)
        findConsistentState(oppoLegalActions, updatedStateByMe, observed, restricted)
        i += 1
      }
      if(restricted.isEmpty) previousOppoScope else restricted.toSet
    }
  }


  def findConsistentState(legalActions: Seq[WondevAction], afterMe: WondevState, observed: WondevState, restricted: ArrayBuffer[Set[Pos]]): Unit = {
    var i = 0
    while (i < legalActions.size) {
      val action = legalActions(i)
      val simulated = WondevArena.next(afterMe, action)
      if (consistent(simulated, observed)) {
        restricted.append(simulated.units.takeRight(2).toSet)
      }
      i += 1
    }
  }

  def consistent(simulated: WondevState, observed: WondevState): Boolean = {
    val observedOppo = observed.units.takeRight(2)
    val simulatedOppo = simulated.units.takeRight(2)
    val observedSelf = observed.units.take(2)
    val simulatedSelf = simulated.units.take(2)
    observedSelf == simulatedSelf &&
      observedOppo.filter(WondevAnalysis.isVisible).forall(simulatedOppo.contains) &&
      hasSameUnvisibleOppo(observedOppo, simulatedOppo, observedSelf) &&
      observed.heightMap == simulated.heightMap
  }

  private def hasSameUnvisibleOppo(observedOppo: Iterable[Pos], simulatedOppo: Iterable[Pos], observedSelf: Iterable[Pos]) = {
    observedOppo.count(pos => !WondevAnalysis.isVisible(pos)) == simulatedOppo.count(pos => observedSelf.forall(_.distance(pos) > 1))
  }

  def removeFog(state: WondevState, oppoScope: Set[Set[Pos]]): WondevState = {
    state.copy(units = state.units.take(2) ++ oppoScope.head.toSeq)
  }

}
