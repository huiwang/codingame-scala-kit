package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevContext, WondevState}

/**
  * Created by hwang on 15/07/2017.
  */
object WarFogAnalysis {


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
      previousOppoScope.filter(oppoSet => {
        val possibleState = previousState.copy(units = myUnits ++ oppoSet.toSeq)
        val updatedStateByMe = WondevArena.next(possibleState, previousAction)
        val oppoLegalActions = WondevArena.nextLegalActions(updatedStateByMe)
        oppoLegalActions.exists(action => consistent(WondevArena.next(updatedStateByMe, action), observed))
      })
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

  private def hasSameUnvisibleOppo(observedOppo: Iterable[Pos], simulatedOppo: Iterable[Pos], observedSelf : Iterable[Pos]) = {
    observedOppo.count(pos => !WondevAnalysis.isVisible(pos)) == simulatedOppo.count(pos => observedSelf.forall(_.distance(pos) > 1))
  }

  def removeFog(state: WondevState, oppoScope: Set[Set[Pos]]): WondevState = {
    state
  }

}
