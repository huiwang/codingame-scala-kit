package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.codingame.challenge.Undoer
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.domain._

import scala.collection.mutable.ArrayBuffer

object UndoWondevArena {

  def next(fromState: FastWondevState, wondevAction: WondevAction): FastWondevState = {
    fromState.undoable.start()
    wondevAction match {
      case MoveBuild(unitIndex, move, build) =>
        fromState.undoable.moveUnit(unitIndex, move)
        if (fromState.readable.isFree(build) || (build == fromState.readable.unitAt(unitIndex))) {
          fromState.undoable.increaseHeight(build)
        }
      case PushBuild(_, build, push) =>
        val pushedUnitIndex = fromState.readable.unitIdOf(build)
        if (fromState.readable.isFree(push)) {
          fromState.undoable.moveUnit(pushedUnitIndex, push)
          fromState.undoable.increaseHeight(build)
        }
      case AcceptDefeat =>
        throw new IllegalStateException("Unable to simulate this defeat action")
    }
    fromState.undoable.swapPlayer()
    fromState.undoable.end()
    fromState
  }

  def nextLegalActions(state: FastWondevState): Seq[WondevAction] = {

    val myStart = if (state.nextPlayer) 0 else 2
    val opStart = if (state.nextPlayer) 2 else 0
    val myUnits = if (state.nextPlayer) state.readable.myUnits else state.readable.opUnits

    val actions: ArrayBuffer[WondevAction] = ArrayBuffer.empty
    var id = myStart
    while (id < myStart + 2) {
      val unit = state.readable.unitAt(id)
      val h = state.readable.heightOf(unit)
      var nid = 0
      val neighbors1 = state.readable.neighborOf(unit)
      while (nid < neighbors1.length) {
        val target1 = neighbors1(nid)
        val h1 = state.readable.heightOf(target1)
        if (WondevContext.isPlayable(h1) && h + 1 >= h1 && state.readable.isFree(target1)) {
          val neighbors2 = state.readable.neighborOf(target1)
          var nid2 = 0
          while (nid2 < neighbors2.length) {
            val target2 = neighbors2(nid2)
            val h2 = state.readable.heightOf(target2)
            if (WondevContext.isPlayable(h2) && (state.readable.isFree(target2) || target2 == unit || myUnits.forall(_.distance(target2) > 1))) {
              actions.append(MoveBuild(id, target1, target2))
            }
            nid2 += 1
          }
        }
        nid += 1
      }

      var oid = opStart
      while (oid < opStart + 2) {
        val target1 = state.readable.unitAt(oid)
        if (WondevAnalysis.isVisible(target1) && unit.distance(target1) == 1) {
          val pushTargets: Array[Pos] = WondevContext.pushTargets(state.size)(unit, target1)

          var pid = 0
          while (pid < pushTargets.length) {
            val target2 = pushTargets(pid)
            val h2 = state.readable.heightOf(target2)
            if (WondevContext.isPlayable(h2) && state.readable.heightOf(target1) + 1 >= h2 && (myUnits.forall(_.distance(target2) > 1) || state.readable.isFree(target2))) {
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
