package com.truelaurel.samplegames.wondev.arena

import com.truelaurel.codingame.challenge.Undoer
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

}
