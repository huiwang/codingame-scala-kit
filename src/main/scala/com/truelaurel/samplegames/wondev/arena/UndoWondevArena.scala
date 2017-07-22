package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.codingame.challenge.Undoer
import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.analysis.WondevAnalysis
import com.truelaurel.samplegames.wondev.domain._

import scala.collection.mutable.ArrayBuffer

object UndoWondevArena {

  def next(fromState: FastWondevState, wondevAction: WondevAction): () => Unit = {
    val undos: ArrayBuffer[() => Unit] = ArrayBuffer.empty
    wondevAction match {
      case MoveBuild(unitIndex, move, build) =>
        undos.append(fromState.moveUnit(unitIndex, move))
        if (fromState.isFree(build) || (build == fromState.unitAt(unitIndex))) {
          undos.append(fromState.increaseHeight(build))
        }
      case PushBuild(_, build, push) =>
        val pushedUnitIndex = fromState.unitIdOf(build)
        if (fromState.isFree(push)) {
          undos.append(fromState.moveUnit(pushedUnitIndex, push))
          undos.append(fromState.increaseHeight(build))
        }
      case AcceptDefeat =>
        throw new IllegalStateException("Unable to simulate this defeat action")
    }
    undos.append(fromState.swapPlayer())
    Undoer.of(undos)
  }

  def nextLegalActions(state: FastWondevState): Seq[WondevAction] = {

    val myStart = if (state.nextPlayer) 0 else 2
    val opStart = if (state.nextPlayer) 2 else 0
    val myUnits = if (state.nextPlayer) state.myUnits else state.opUnits

    val actions: ArrayBuffer[WondevAction] = ArrayBuffer.empty
    var id = myStart
    while (id < myStart + 2) {
      val unit = state.unitAt(id)
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
        val target1 = state.unitAt(oid)
        if (WondevAnalysis.isVisible(target1) && unit.distance(target1) == 1) {
          val pushTargets: Array[Pos] = WondevContext.pushTargets(state.size)(unit, target1)

          var pid = 0
          while (pid < pushTargets.length) {
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
