package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.arena.WondevArena
import com.truelaurel.samplegames.wondev.domain.{WondevAction, WondevContext, WondevState}

/**
  * Created by hwang on 15/07/2017.
  */
object WarFogAnalysis {


  def restrictOppoScope(state: WondevState,
                        previousState: WondevState,
                        previousAction: WondevAction,
                        previousOppoScope: Iterator[Set[Pos]]): Iterator[Set[Pos]] = {
    val myUnits = state.units.take(2)
    val visibleOppo = state.units.takeRight(2).filter(WondevAnalysis.isVisible)
    if (previousAction == null) {
      val occupables = state.heightMap.keySet.filter(
        pos => WondevContext.isPlayable(state.heightOf(pos)) &&
          myUnits.forall(myPos => myPos.distance(pos) > 0)
      )
      val oppoScope = occupables.subsets(2)
      val increased = state.heightMap.find(_._2 == 1)
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
        oppoLegalActions.exists(action => consistent(WondevArena.next(updatedStateByMe, action), state))
      })
    }
  }

  def consistent(simulated: WondevState, observed: WondevState): Boolean = {
    observed.units.take(2) == simulated.units.take(2) &&
      observed.units.takeRight(2).filter(WondevAnalysis.isVisible).forall(simulated.units.takeRight(2).contains) &&
      observed.heightMap == simulated.heightMap
  }

  def removeFog(state: WondevState, oppoScope: Iterator[Set[Pos]]): WondevState = {
    state
  }

}
