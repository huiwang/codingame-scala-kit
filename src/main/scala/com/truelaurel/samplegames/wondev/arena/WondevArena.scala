package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.domain._

import scala.collection.mutable.ArrayBuffer

/**
  * Created by hwang on 24/06/2017.
  */
object WondevArena {
  def next(fromState: WondevState, action: WondevAction): WondevState = action match {
    case MoveBuild(unitIndex, move, build) =>
      fromState.copy(
        units = fromState.units.updated(unitIndex, move),
        heightMap = {
          if (fromState.isFree(build) || (build == fromState.units(unitIndex))) {
            fromState.heightMap.updated(build, fromState.heightOf(build) + 1)
          } else {
            fromState.heightMap
          }
        },
        nextPlayer = !fromState.nextPlayer
      )

    case PushBuild(unitIndex, build, push) =>
      val pushedUnitIndex = fromState.units.indexOf(build)
      if (fromState.isFree(push)) {
        fromState.copy(
          units = fromState.units.updated(pushedUnitIndex, push),
          heightMap = fromState.heightMap.updated(build, fromState.heightOf(build) + 1),
          nextPlayer = !fromState.nextPlayer
        )
      } else {
        fromState.copy(nextPlayer = !fromState.nextPlayer)
      }
    case AcceptDefeat =>
      throw new IllegalStateException("Unable to simulate this defeat action")
  }

  def nextLegalActions(state: WondevState): Seq[WondevAction] = {

    val myStart = if (state.nextPlayer) 0 else 2
    val opStart = if (state.nextPlayer) 2 else 0
    val myUnits = state.units.slice(myStart, myStart + 2)

    val actions: ArrayBuffer[WondevAction] = ArrayBuffer.empty
    var id = myStart
    while (id < myStart + 2) {
      val unit = state.units(id)
      val h = state.heightOf(unit)
      var nid = 0
      val neighbors1 = state.neighborOf(unit)
      while (nid < neighbors1.length) {
        val target1 = neighbors1(nid)
        val h1 = state.heightOf(target1)
        if (WondevContext.isPlayable(h1) && h + 1 >= h1 && state.isFree(target1)) {
          val neighbors2 = state.neighborOf(target1)
          var nid2 = 0
          while (nid2 < neighbors2.length) {
            val target2 = neighbors2(nid2)
            val h2 = state.heightOf(target2)
            if (WondevContext.isPlayable(h2) && (state.isFree(target2) || target2 == unit || myUnits.forall(_.distance(target2) > 1))) {
              actions.append(MoveBuild(id, target1, target2))
            }
            nid2 += 1
          }
        }
        nid += 1
      }

      var oid = opStart
      while (oid < opStart + 2) {
        val target1 = state.units(oid)
        if(WondevAnalysis.isVisible(target1) && unit.distance(target1) == 1) {
          val pushTargets: Array[Pos] = WondevContext.pushTargets(state.size)(unit, target1)

          var pid = 0
          while(pid < pushTargets.length) {
            val target2 = pushTargets(pid)
            val h2 = state.heightOf(target2)
            if (WondevContext.isPlayable(h2) && state.heightOf(target1) + 1 >= h2 && (myUnits.forall(_.distance(target2) > 1) || state.isFree(target2))) {
              actions.append(PushBuild(id, target1, target2))
            }
            pid += 1
          }
        }

        oid += 1
      }
      id += 1
    }
    actions
  }


}
